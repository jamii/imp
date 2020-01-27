use crate::shared::*;

#[derive(Debug, Clone)]
pub struct ValueBir {
    pub name: Name,
    pub typ: ValueType,
    pub args: Vec<ScalarRef>,
    pub body: BooleanBir,
}

#[derive(Debug, Clone)]
pub enum BooleanBir {
    None,
    Some,
    Union(Box<BooleanBir>, Box<BooleanBir>),
    Intersect(Box<BooleanBir>, Box<BooleanBir>),
    If(Box<BooleanBir>, Box<BooleanBir>, Box<BooleanBir>),
    ScalarEqual(ScalarRef, ScalarRef),
    Negate(Box<BooleanBir>),
    Apply(ValueRef, Vec<ScalarRef>),
}

#[derive(Debug, Clone)]
pub enum ValueRef {
    Name(Name),
    Native(Native),
}

#[derive(Debug, Clone)]
pub enum ScalarRef {
    Name(Name),
    Scalar(Scalar),
}

#[derive(Debug)]
struct BirContext<'a> {
    birs: RefCell<&'a mut Vec<ValueBir>>,
    gensym: &'a Gensym,
    type_cache: &'a Cache<ValueType>,
}

type Bindings = Environment<Binding>;

#[derive(Debug, Clone)]
enum Binding {
    Abstract,
    LetFun { bir: BooleanBir, args: Vec<Name> },
    LetSet { name: Name, scope_names: Vec<Name> },
}

impl<'a> BirContext<'a> {
    fn define(
        &self,
        name: Option<Name>,
        typ: ValueType,
        args: Vec<Name>,
        body: BooleanBir,
    ) -> Name {
        let name = name.unwrap_or_else(|| self.gensym.name());
        let bir = ValueBir {
            name: name.clone(),
            typ,
            args: args.into_iter().map(|name| ScalarRef::Name(name)).collect(),
            body,
        };
        println!("{}", bir);
        self.birs.borrow_mut().push(bir);
        name
    }

    fn as_scalar_ref(&self, env: &Bindings, expr: &Expression) -> Option<ScalarRef> {
        if let Expression::Scalar(scalar) = expr {
            return Some(ScalarRef::Scalar(scalar.clone()));
        }
        if let Expression::Name(name) = expr {
            if let Binding::Abstract = env.lookup(name).unwrap() {
                return Some(ScalarRef::Name(name.clone()));
            }
        }
        None
    }
}

impl Expression {
    pub fn bir(&self, type_cache: &Cache<ValueType>, gensym: &Gensym) -> Vec<ValueBir> {
        let mut birs = vec![];
        let arg_names = gensym.names(type_cache.get(self).arity().unwrap_or(0));
        let context = BirContext {
            birs: RefCell::new(&mut birs),
            gensym,
            type_cache,
        };
        let body = self.bir_into(&context, &Environment::new(), &BooleanBir::Some, &arg_names);
        birs.push(ValueBir {
            name: "<main>".to_owned(),
            typ: type_cache.get(self).clone(),
            args: arg_names
                .into_iter()
                .map(|name| ScalarRef::Name(name))
                .collect(),
            body,
        });
        birs
    }

    fn bir_into(
        &self,
        context: &BirContext,
        env: &Bindings,
        scope: &BooleanBir,
        arg_names: &[Name],
    ) -> BooleanBir {
        use BooleanBir as B;
        use Expression::*;
        let self_arity = context.type_cache.get(self).arity();
        println!("self: {}", self);
        dbg!(arg_names);
        match self_arity {
            Option::None => return B::None,
            Option::Some(self_arity) => assert_eq!(self_arity, arg_names.len()),
        }
        let bir = match self {
            None => B::None,
            Some => B::Some,
            Scalar(scalar) => B::ScalarEqual(
                ScalarRef::Scalar(scalar.clone()),
                ScalarRef::Name(arg_names[0].clone()),
            ),
            Union(a, b) => {
                let a_bir = a.bir_into(context, env, scope, arg_names);
                let b_bir = if self_arity.unwrap() == 0 {
                    // can only reach b if !a
                    let scope = B::Intersect(box scope.clone(), box B::Negate(box a_bir.clone()));
                    b.bir_into(context, env, &scope, arg_names)
                } else {
                    b.bir_into(context, env, scope, arg_names)
                };
                B::Union(box a_bir, box b_bir)
            }
            Intersect(a, b) => {
                let a_bir = a.bir_into(context, env, scope, arg_names);
                // can only reach b if exists a
                let scope = B::Intersect(box scope.clone(), box a_bir.clone());
                let b_bir = b.bir_into(context, env, &scope, arg_names);
                B::Intersect(box a_bir, box b_bir)
            }
            Product(a, b) => {
                let a_arity = context.type_cache.get(a).arity().unwrap();
                let a_bir = a.bir_into(context, env, scope, &arg_names[..a_arity]);
                // can only reach b if exists a
                let scope = B::Intersect(box scope.clone(), box a_bir.clone());
                let b_bir = b.bir_into(context, env, &scope, &arg_names[a_arity..]);
                B::Intersect(box a_bir, box b_bir)
            }
            Equal(a, b) => {
                match (context.as_scalar_ref(env, a), context.as_scalar_ref(env, b)) {
                    (Option::Some(a_ref), Option::Some(b_ref)) => B::ScalarEqual(a_ref, b_ref),
                    _ => {
                        let ab_arity = context.type_cache.get(a).arity().unwrap_or(0);
                        let scope_names = a
                            .free_names()
                            .union(&b.free_names())
                            .cloned()
                            .filter(|name| {
                                if let Option::Some(Binding::Abstract) = env.lookup(name) {
                                    true
                                } else {
                                    false
                                }
                            })
                            .collect::<Vec<_>>();
                        let arg_names = context.gensym.names(ab_arity);
                        let self_names = scope_names
                            .iter()
                            .chain(arg_names.iter())
                            .cloned()
                            .collect::<Vec<_>>();
                        let a_name = context.define(
                            Option::None,
                            context.type_cache.get(a).clone(),
                            self_names.clone(),
                            B::Intersect(
                                box scope.clone(),
                                box a.bir_into(context, env, scope, &arg_names),
                            ),
                        );
                        let b_name = context.define(
                            Option::None,
                            context.type_cache.get(b).clone(),
                            self_names.clone(),
                            B::Intersect(
                                box scope.clone(),
                                box b.bir_into(context, env, scope, &arg_names),
                            ),
                        );

                        // !(((a n) & ! (b n)) | ((b n) & !(a n)))
                        let self_refs = self_names
                            .iter()
                            .map(|name| ScalarRef::Name(name.clone()))
                            .collect::<Vec<_>>();
                        B::Negate(box B::Union(
                            box B::Intersect(
                                box B::Apply(ValueRef::Name(a_name.clone()), self_refs.clone()),
                                box B::Negate(box B::Apply(
                                    ValueRef::Name(b_name.clone()),
                                    self_refs.clone(),
                                )),
                            ),
                            box B::Intersect(
                                box B::Apply(ValueRef::Name(b_name.clone()), self_refs.clone()),
                                box B::Negate(box B::Apply(
                                    ValueRef::Name(a_name.clone()),
                                    self_refs.clone(),
                                )),
                            ),
                        ))
                    }
                }
            }
            Negate(a) => {
                let a_arity = context.type_cache.get(a).arity().unwrap_or(0);
                let arg_names = context.gensym.names(a_arity);
                B::Negate(box a.bir_into(context, env, scope, &arg_names))
            }
            Name(name) => match env.lookup(name).unwrap() {
                Binding::Abstract => B::ScalarEqual(
                    ScalarRef::Name(name.clone()),
                    ScalarRef::Name(arg_names[0].clone()),
                ),
                Binding::LetFun { bir, args } => {
                    let mut bir = bir.clone();
                    assert_eq!(args.len(), arg_names.len());
                    for (arg1, arg2) in args.iter().zip(arg_names.iter()) {
                        bir = B::Intersect(
                            box B::ScalarEqual(
                                ScalarRef::Name(arg1.clone()),
                                ScalarRef::Name(arg2.clone()),
                            ),
                            box bir,
                        );
                    }
                    bir
                }
                Binding::LetSet { name, scope_names } => B::Apply(
                    ValueRef::Name(name.clone()),
                    scope_names
                        .iter()
                        .chain(arg_names.iter())
                        .map(|name| ScalarRef::Name(name.clone()))
                        .collect(),
                ),
            },
            Let(name, value, body) => {
                let value_arg_names = context
                    .gensym
                    .names(context.type_cache.get(value).arity().unwrap_or(0));
                let value_bir = value.bir_into(context, &env, scope, &value_arg_names);
                let mut env = env.clone();
                if context.type_cache.get(value).is_function() {
                    env.bind(
                        name.clone(),
                        Binding::LetFun {
                            bir: value_bir,
                            args: value_arg_names,
                        },
                    );
                } else {
                    let scope_names = value
                        .free_names()
                        .into_iter()
                        .filter(|name| {
                            if let Option::Some(Binding::Abstract) = env.lookup(name) {
                                true
                            } else {
                                false
                            }
                        })
                        .collect::<Vec<_>>();
                    context.define(
                        Option::Some(name.clone()),
                        context.type_cache.get(value).clone(),
                        scope_names
                            .iter()
                            .chain(value_arg_names.iter())
                            .cloned()
                            .collect(),
                        B::Intersect(box scope.clone(), box value_bir),
                    );
                    env.bind(
                        name.clone(),
                        Binding::LetSet {
                            name: name.clone(),
                            scope_names: scope_names.to_vec(),
                        },
                    );
                }
                body.bir_into(context, &env, scope, arg_names)
            }
            If(cond, if_true, if_false) => {
                let cond_bir = cond.bir_into(context, env, scope, &[]);
                let if_true_bir = {
                    // can only reach if_true if cond
                    let scope = B::Intersect(box scope.clone(), box cond_bir.clone());
                    if_true.bir_into(context, env, &scope, arg_names)
                };
                let if_false_bir = {
                    // can only reach if_false if !cond
                    let scope =
                        B::Intersect(box scope.clone(), box B::Negate(box cond_bir.clone()));
                    if_false.bir_into(context, env, &scope, arg_names)
                };
                B::If(box cond_bir, box if_true_bir, box if_false_bir)
            }
            Abstract(name, body) => {
                let mut env = env.clone();
                env.bind(name.clone(), Binding::Abstract);
                let scope = B::Intersect(
                    box scope.clone(),
                    box B::ScalarEqual(
                        ScalarRef::Name(name.clone()),
                        ScalarRef::Name(arg_names[0].clone()),
                    ),
                );
                B::Intersect(
                    box B::ScalarEqual(
                        ScalarRef::Name(name.clone()),
                        ScalarRef::Name(arg_names[0].clone()),
                    ),
                    box body.bir_into(context, &env, &scope, &arg_names[1..]),
                )
            }
            Apply(a, b) => {
                let mut a = &**a;
                let mut b = &**b;
                if context.type_cache.get(a).is_function() {
                    std::mem::swap(&mut a, &mut b);
                }
                let a_arity = context.type_cache.get(a).arity().unwrap_or(0);
                let b_arity = context.type_cache.get(b).arity().unwrap_or(0);
                let min_arity = a_arity.min(b_arity);
                let applied_arg_names = context.gensym.names(min_arity);
                let mut a_arg_names = applied_arg_names.clone();
                let mut b_arg_names = applied_arg_names.clone();
                if a_arity > b_arity {
                    a_arg_names.extend(arg_names.iter().cloned());
                } else {
                    b_arg_names.extend(arg_names.iter().cloned());
                }
                let a_bir = a.bir_into(context, env, scope, &a_arg_names);
                let b_bir = if context.type_cache.get(b).is_function() {
                    let scope = B::Intersect(box scope.clone(), box a_bir.clone());
                    b.bir_into(context, env, &scope, &b_arg_names)
                } else {
                    b.bir_into(context, env, scope, &b_arg_names)
                };
                B::Intersect(box a_bir, box b_bir)
            }
            Native(native) => B::Apply(
                ValueRef::Native(native.clone()),
                arg_names
                    .iter()
                    .map(|name| ScalarRef::Name(name.clone()))
                    .collect(),
            ),
            Reduce(..) | Seal(..) | Unseal(..) | Solve(..) => panic!("Unimplemented: {:?}", self),
        };

        println!("self: {}  =>  bir: {}", self, bir);

        bir
    }
}

impl ValueBir {
    pub fn validate(&self) -> Result<(), String> {
        let mut bound = Some(HashSet::new());
        self.body
            .validate(&mut bound)
            .map_err(|s| format!("{} in {}", s, self))?;
        if let Some(bound) = bound {
            for arg in &self.args {
                if let ScalarRef::Name(name) = arg {
                    if !bound.contains(name) {
                        return Err(format!("arg {} not bound in {}", arg, self));
                    }
                }
            }
        }
        Ok(())
    }
}

impl BooleanBir {
    fn validate<'a>(&'a self, bound: &mut Option<HashSet<&'a Name>>) -> Result<(), String> {
        use BooleanBir::*;

        fn intersect_bound<'a>(
            a_bound: Option<HashSet<&'a Name>>,
            b_bound: Option<HashSet<&'a Name>>,
        ) -> Option<HashSet<&'a Name>> {
            match (a_bound, b_bound) {
                (Option::Some(a_bound), Option::Some(b_bound)) => {
                    Option::Some(a_bound.intersection(&b_bound).map(|name| &**name).collect())
                }
                (Option::Some(bound), Option::None) | (Option::None, Option::Some(bound)) => {
                    Option::Some(bound)
                }
                (Option::None, Option::None) => Option::None,
            }
        }

        match self {
            None => {
                *bound = Option::None;
            }
            Some => (),
            Union(a, b) => {
                let mut a_bound = bound.clone();
                let mut b_bound = bound.clone();
                a.validate(&mut a_bound)?;
                b.validate(&mut b_bound)?;
                *bound = intersect_bound(a_bound, b_bound);
            }
            Intersect(a, b) => {
                a.validate(bound)?;
                b.validate(bound)?;
            }
            If(a, b, c) => {
                let mut b_bound = bound.clone();
                let mut c_bound = bound.clone();
                a.validate(&mut b_bound)?;
                b.validate(&mut b_bound)?;
                c.validate(&mut c_bound)?;
                *bound = intersect_bound(b_bound, c_bound);
            }
            ScalarEqual(a, b) => match (a, b) {
                (ScalarRef::Scalar(_), ScalarRef::Scalar(_)) => (),
                (ScalarRef::Name(name), ScalarRef::Scalar(_))
                | (ScalarRef::Scalar(_), ScalarRef::Name(name)) => {
                    if let Option::Some(bound) = bound {
                        bound.insert(name);
                    }
                }
                (ScalarRef::Name(name_a), ScalarRef::Name(name_b)) => {
                    if let Option::Some(bound) = bound {
                        if bound.contains(name_a) {
                            bound.insert(name_b);
                        } else if bound.contains(name_b) {
                            bound.insert(name_a);
                        } else {
                            return Err(format!("{}={} but neither bound yet", name_a, name_b));
                        }
                    }
                }
            },
            Negate(_) => (),
            Apply(v, args) => match v {
                ValueRef::Name(_) => {
                    if let Option::Some(bound) = bound {
                        for arg in args {
                            if let ScalarRef::Name(name) = arg {
                                bound.insert(name);
                            }
                        }
                    }
                }
                ValueRef::Native(native) => {
                    if let Option::Some(bound) = bound {
                        if args[0..native.input_arity].iter().all(|arg| match arg {
                            ScalarRef::Scalar(_) => true,
                            ScalarRef::Name(name) => bound.contains(name),
                        }) {
                            for arg in &args[native.input_arity..] {
                                if let ScalarRef::Name(name) = arg {
                                    bound.insert(name);
                                }
                            }
                        } else {
                            return Err(format!("{} but input not bound yet", v));
                        }
                    }
                }
            },
        }
        Ok(())
    }
}
