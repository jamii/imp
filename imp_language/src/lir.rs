use crate::shared::*;

// Logical Intermediate Representation

// TODO can inline any ValueLir that is only used once
// TODO can inline any ValueLir that is used monotonically in only one other ValueLir

#[derive(Debug, Clone)]
pub struct ValueLir {
    pub name: Name,
    pub typ: ValueType,
    pub args: Vec<Name>,
    pub body: BooleanLir,
}

#[derive(Debug, Clone)]
pub enum BooleanLir {
    None,
    Some,
    Union(Box<BooleanLir>, Box<BooleanLir>),
    Intersect(Box<BooleanLir>, Box<BooleanLir>),
    If(Box<BooleanLir>, Box<BooleanLir>, Box<BooleanLir>),
    ScalarEqual(ScalarRef, ScalarRef),
    Negate(Box<BooleanLir>),
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

#[derive(Debug, Clone)]
pub struct Lirs {
    pub lirs: Vec<ValueLir>,
}

#[derive(Debug)]
struct LirContext<'a> {
    lirs: RefCell<&'a mut Vec<ValueLir>>,
    gensym: &'a Gensym,
    type_cache: &'a Cache<ValueType>,
}

type Bindings<'a> = Environment<Binding<'a>>;

#[derive(Debug, Clone)]
enum Binding<'a> {
    Constant(bool),
    Abstract,
    LetFun { expr: &'a Expression },
    LetSet { name: Name, scope_names: Vec<Name> },
}

impl<'a> LirContext<'a> {
    fn define(
        &self,
        name: Option<Name>,
        typ: ValueType,
        args: Vec<Name>,
        body: BooleanLir,
    ) -> Name {
        let name = name.unwrap_or_else(|| self.gensym.name());
        let lir = ValueLir {
            name: name.clone(),
            typ,
            args,
            body: body.simplify(),
        };
        debug!("{}", lir);
        self.lirs.borrow_mut().push(lir);
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
    pub fn lirs(&self, type_cache: &Cache<ValueType>, gensym: &Gensym) -> Lirs {
        let mut lirs = vec![];
        let arg_names = gensym.names(type_cache.get(self).arity().unwrap_or(0));
        let context = LirContext {
            lirs: RefCell::new(&mut lirs),
            gensym,
            type_cache,
        };
        let body = self.lir_into(&context, &Environment::new(), &BooleanLir::Some, &arg_names);
        context.define(
            Option::Some("<main>".to_owned()),
            type_cache.get(self).clone(),
            arg_names,
            body,
        );
        Lirs { lirs }
    }

    fn lir_into(
        &self,
        context: &LirContext,
        env: &Bindings,
        scope: &BooleanLir,
        arg_names: &[Name],
    ) -> BooleanLir {
        use BooleanLir as B;
        use Expression::*;
        let self_type = context.type_cache.get(self);
        let self_arity = self_type.arity();
        debug!("self: {}", self);
        d!(arg_names);
        match self_arity {
            Option::None => return B::None,
            Option::Some(self_arity) => assert_eq!(self_arity, arg_names.len()),
        }
        let lir = match self {
            None => B::None,
            Some => B::Some,
            Scalar(scalar) => B::ScalarEqual(
                ScalarRef::Scalar(scalar.clone()),
                ScalarRef::Name(arg_names[0].clone()),
            ),
            Union(a, b) => {
                let a_lir = a.lir_into(context, env, scope, arg_names);
                let b_lir = if self_arity.unwrap() == 0 {
                    // can only reach b if !a
                    let scope = B::Intersect(box scope.clone(), box B::Negate(box a_lir.clone()));
                    b.lir_into(context, env, &scope, arg_names)
                } else {
                    b.lir_into(context, env, scope, arg_names)
                };
                B::Union(box a_lir, box b_lir)
            }
            Intersect(a, b) => {
                let a_lir = a.lir_into(context, env, scope, arg_names);
                // can only reach b if exists a
                let scope = B::Intersect(box scope.clone(), box a_lir.clone());
                let b_lir = b.lir_into(context, env, &scope, arg_names);
                B::Intersect(box a_lir, box b_lir)
            }
            Product(a, b) => {
                let a_arity = context.type_cache.get(a).arity().unwrap();
                let a_lir = a.lir_into(context, env, scope, &arg_names[..a_arity]);
                // can only reach b if exists a
                let scope = B::Intersect(box scope.clone(), box a_lir.clone());
                let b_lir = b.lir_into(context, env, &scope, &arg_names[a_arity..]);
                B::Intersect(box a_lir, box b_lir)
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
                                box a.lir_into(context, env, scope, &arg_names),
                            ),
                        );
                        let b_name = context.define(
                            Option::None,
                            context.type_cache.get(b).clone(),
                            self_names.clone(),
                            B::Intersect(
                                box scope.clone(),
                                box b.lir_into(context, env, scope, &arg_names),
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
                B::Negate(box a.lir_into(context, env, scope, &arg_names))
            }
            Name(name) => match env.lookup(name).unwrap() {
                Binding::Constant(b) => {
                    if *b {
                        B::Some
                    } else {
                        B::None
                    }
                }
                Binding::Abstract => B::ScalarEqual(
                    ScalarRef::Name(name.clone()),
                    ScalarRef::Name(arg_names[0].clone()),
                ),
                Binding::LetFun { expr } => expr.lir_into(context, env, scope, arg_names),
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
                let mut env = env.clone();
                if context.type_cache.get(value).is_function() {
                    env.bind(name.clone(), Binding::LetFun { expr: value });
                } else {
                    let value_arg_names = context
                        .gensym
                        .names(context.type_cache.get(value).arity().unwrap_or(0));
                    let value_lir = value.lir_into(context, &env, scope, &value_arg_names);
                    if let B::None = value_lir {
                        env.bind(name.clone(), Binding::Constant(false));
                    } else if let B::Some = value_lir {
                        env.bind(name.clone(), Binding::Constant(true));
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
                            B::Intersect(box scope.clone(), box value_lir),
                        );
                        env.bind(
                            name.clone(),
                            Binding::LetSet {
                                name: name.clone(),
                                scope_names: scope_names.to_vec(),
                            },
                        );
                    }
                }
                body.lir_into(context, &env, scope, arg_names)
            }
            If(cond, if_true, if_false) => {
                let cond_lir = cond.lir_into(context, env, scope, &[]);
                let if_true_lir = {
                    // can only reach if_true if cond
                    let scope = B::Intersect(box scope.clone(), box cond_lir.clone());
                    if_true.lir_into(context, env, &scope, arg_names)
                };
                let if_false_lir = {
                    // can only reach if_false if !cond
                    let scope =
                        B::Intersect(box scope.clone(), box B::Negate(box cond_lir.clone()));
                    if_false.lir_into(context, env, &scope, arg_names)
                };
                B::If(box cond_lir, box if_true_lir, box if_false_lir)
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
                    box body.lir_into(context, &env, &scope, &arg_names[1..]),
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
                let a_lir = a.lir_into(context, env, scope, &a_arg_names);
                let b_lir = if context.type_cache.get(b).is_function() {
                    let scope = B::Intersect(box scope.clone(), box a_lir.clone());
                    b.lir_into(context, env, &scope, &b_arg_names)
                } else {
                    b.lir_into(context, env, scope, &b_arg_names)
                };
                B::Intersect(box a_lir, box b_lir)
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

        debug!("self: {}  =>  lir: {}", self, lir);

        lir
    }
}

impl ValueLir {
    pub fn validate(&self) -> Result<(), String> {
        match self.body {
            BooleanLir::None => (),
            BooleanLir::Some => {
                if self.args.len() > 0 {
                    Err(format!("All args unbound - body is some"))?
                }
            }
            _ => {
                let mut bound = HashSet::new();
                self.body
                    .validate(&mut bound)
                    .map_err(|s| format!("{} in {}", s, self))?;
                for arg in &self.args {
                    if !bound.contains(arg) {
                        return Err(format!("arg {} not bound in {}", arg, self));
                    }
                }
            }
        }
        Ok(())
    }
}

impl BooleanLir {
    fn simplify(&self) -> BooleanLir {
        use BooleanLir::*;
        match self {
            None => None,
            Some => Some,
            Union(a, b) => match (a.simplify(), b.simplify()) {
                (None, c) | (c, None) => c,
                (Some, _) | (_, Some) => Some,
                (a, b) => Union(box a, box b),
            },
            Intersect(a, b) => match (a.simplify(), b.simplify()) {
                (None, _) | (_, None) => None,
                (Some, c) | (c, Some) => c,
                (a, b) => Intersect(box a, box b),
            },
            If(c, t, f) => match (c.simplify(), t.simplify(), f.simplify()) {
                (None, _, f) => f,
                (Some, t, _) => t,
                (_, None, None) => None,
                (c, None, Some) => Negate(box c),
                (c, Some, None) => c,
                (_, Some, Some) => Some,
                (c, t, None) => Intersect(box c, box t),
                (c, None, f) => Intersect(box Negate(box c), box f),
                (c, t, Some) => Union(box Negate(box c), box t),
                (c, Some, f) => Union(box c, box f),
                (c, t, f) => If(box c, box t, box f),
            },
            ScalarEqual(a, b) => match (a, b) {
                (ScalarRef::Scalar(a), ScalarRef::Scalar(b)) => {
                    if a == b {
                        Some
                    } else {
                        None
                    }
                }
                _ => ScalarEqual(a.clone(), b.clone()),
            },
            Negate(a) => match a.simplify() {
                None => Some,
                Some => None,
                a => Negate(box a),
            },
            Apply(f, args) => Apply(f.clone(), args.clone()),
        }
    }

    fn validate<'a>(&'a self, bound: &mut HashSet<&'a Name>) -> Result<(), String> {
        use BooleanLir::*;
        match self {
            None | Some => Err(format!("{} nested in lir", self))?,
            Union(a, b) => {
                let mut a_bound = bound.clone();
                let mut b_bound = bound.clone();
                a.validate(&mut a_bound)?;
                b.validate(&mut b_bound)?;
                *bound = a_bound.intersection(&b_bound).map(|n| &**n).collect();
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
                *bound = b_bound.intersection(&c_bound).map(|n| &**n).collect();
            }
            ScalarEqual(a, b) => match (a, b) {
                (ScalarRef::Scalar(_), ScalarRef::Scalar(_)) => (),
                (ScalarRef::Name(name), ScalarRef::Scalar(_))
                | (ScalarRef::Scalar(_), ScalarRef::Name(name)) => {
                    bound.insert(name);
                }
                (ScalarRef::Name(name_a), ScalarRef::Name(name_b)) => {
                    if bound.contains(name_a) {
                        bound.insert(name_b);
                    } else if bound.contains(name_b) {
                        bound.insert(name_a);
                    } else {
                        return Err(format!("{}={} but neither bound yet", name_a, name_b));
                    }
                }
            },
            Negate(_) => (),
            Apply(v, args) => match v {
                ValueRef::Name(_) => {
                    for arg in args {
                        if let ScalarRef::Name(name) = arg {
                            bound.insert(name);
                        }
                    }
                }
                ValueRef::Native(native) => {
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
            },
        }
        Ok(())
    }
}
