use crate::shared::*;

#[derive(Debug, Clone)]
pub struct ValueBir {
    pub name: Name,
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
    Exists(Vec<Name>, Box<BooleanBir>),
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
    LetFun {
        expr: Expression,
        bindings: Bindings,
    },
    LetSet {
        name: Name,
        scope_names: Vec<Name>,
    },
}

impl<'a> BirContext<'a> {
    fn define(
        &self,
        name: Option<Name>,
        args: Vec<Name>,
        scope: Name,
        scope_names: Vec<Name>,
        body: BooleanBir,
    ) -> Name {
        let name = name.unwrap_or_else(|| self.gensym.name());
        println!("def \n{} =\n{} ->\n{}", name, args.join(" "), body);
        self.birs.borrow_mut().push(ValueBir {
            name: name.clone(),
            args: args.into_iter().map(|name| ScalarRef::Name(name)).collect(),
            body: BooleanBir::Intersect(
                box BooleanBir::Apply(
                    ValueRef::Name(scope),
                    scope_names
                        .into_iter()
                        .map(|name| ScalarRef::Name(name))
                        .collect(),
                ),
                box body,
            ),
        });
        name
    }

    fn narrow_scope(&self, scope: Name, scope_names: Vec<Name>, body: BooleanBir) -> Name {
        self.define(None, scope_names.clone(), scope, scope_names, body)
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
        let context = BirContext {
            birs: RefCell::new(&mut birs),
            gensym,
            type_cache,
        };
        let body = self.bir_into(
            &context,
            &Environment::new(),
            &"scope0".to_owned(),
            &[],
            &[],
        );
        birs.push(ValueBir {
            name: "<main>".to_owned(),
            args: vec![],
            body,
        });
        birs
    }

    fn bir_into(
        &self,
        context: &BirContext,
        env: &Bindings,
        scope: &Name,
        scope_names: &[Name],
        arg_names: &[Name],
    ) -> BooleanBir {
        use BooleanBir as B;
        use Expression::*;

        let self_arity = context.type_cache.get(self).arity();
        assert_eq!(self_arity.unwrap_or(0), arg_names.len());
        match self {
            _ if self_arity.is_none() => B::None,
            None => B::None,
            Some => B::Some,
            Scalar(scalar) => B::ScalarEqual(
                ScalarRef::Scalar(scalar.clone()),
                ScalarRef::Name(arg_names[0].clone()),
            ),
            Union(a, b) => {
                let bir_a = a.bir_into(context, env, scope, scope_names, arg_names);
                let bir_b = if self_arity.unwrap() == 0 {
                    // can only reach b if !a
                    let scope = context.narrow_scope(
                        scope.clone(),
                        scope_names.to_vec(),
                        B::Negate(box bir_a.clone()),
                    );
                    b.bir_into(context, env, &scope, scope_names, arg_names)
                } else {
                    b.bir_into(context, env, scope, scope_names, arg_names)
                };
                B::Union(box bir_a, box bir_b)
            }
            Intersect(a, b) => {
                let bir_a = a.bir_into(context, env, scope, scope_names, arg_names);
                // can only reach b if exists a
                let scope = context.narrow_scope(
                    scope.clone(),
                    scope_names.to_vec(),
                    B::Exists(arg_names.to_vec(), box bir_a.clone()),
                );
                let bir_b = b.bir_into(context, env, &scope, scope_names, arg_names);
                B::Intersect(box bir_a, box bir_b)
            }
            Product(a, b) => {
                let bir_a = a.bir_into(context, env, scope, scope_names, arg_names);
                // can only reach b if exists a
                let scope = context.narrow_scope(
                    scope.clone(),
                    scope_names.to_vec(),
                    B::Exists(arg_names.to_vec(), box bir_a.clone()),
                );
                let bir_b = b.bir_into(context, env, &scope, scope_names, arg_names);
                B::Intersect(box bir_a, box bir_b)
            }
            Equal(a, b) => {
                match (context.as_scalar_ref(env, a), context.as_scalar_ref(env, b)) {
                    (Option::Some(a_ref), Option::Some(b_ref)) => B::ScalarEqual(a_ref, b_ref),
                    _ => {
                        let ab_arity = context.type_cache.get(a).arity().unwrap_or(0);
                        let arg_names = context.gensym.names(ab_arity);
                        let self_names = scope_names
                            .iter()
                            .chain(arg_names.iter())
                            .cloned()
                            .collect::<Vec<_>>();
                        let a_name = context.define(
                            Option::None,
                            self_names.clone(),
                            scope.clone(),
                            scope_names.to_vec(),
                            a.bir_into(context, env, scope, scope_names, &arg_names),
                        );
                        let b_name = context.define(
                            Option::None,
                            self_names.clone(),
                            scope.clone(),
                            scope_names.to_vec(),
                            b.bir_into(context, env, scope, scope_names, &arg_names),
                        );

                        // !exists(n -> ((a n) & ! (b n)) | ((b n) & !(a n)))
                        let self_refs = self_names
                            .iter()
                            .map(|name| ScalarRef::Name(name.clone()))
                            .collect::<Vec<_>>();
                        B::Negate(box B::Exists(
                            arg_names.to_vec(),
                            box B::Union(
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
                            ),
                        ))
                    }
                }
            }
            Negate(a) => {
                let a_arity = context.type_cache.get(a).arity().unwrap_or(0);
                let arg_names = context.gensym.names(a_arity);
                B::Negate(box B::Exists(
                    arg_names.clone(),
                    box a.bir_into(context, env, scope, scope_names, &arg_names),
                ))
            }
            Name(name) => match env.lookup(name).unwrap() {
                Binding::Abstract => {
                    assert!(scope_names.iter().find(|name2| name == *name2).is_some());
                    B::ScalarEqual(
                        ScalarRef::Name(name.clone()),
                        ScalarRef::Name(arg_names[0].clone()),
                    )
                }
                Binding::LetFun {
                    expr,
                    bindings: fun_env,
                } => expr.bir_into(context, fun_env, scope, scope_names, arg_names),
                Binding::LetSet {
                    name,
                    scope_names: set_scope_names,
                } => B::Apply(
                    ValueRef::Name(name.clone()),
                    set_scope_names
                        .iter()
                        .chain(arg_names.iter())
                        .map(|name| ScalarRef::Name(name.clone()))
                        .collect(),
                ),
            },
            Let(name, value, body) => {
                let mut env = env.clone();
                if context.type_cache.get(value).is_function() {
                    let bindings = env.clone();
                    env.bind(
                        name.clone(),
                        Binding::LetFun {
                            expr: (**value).clone(),
                            bindings,
                        },
                    );
                } else {
                    let bir_value = value.bir_into(context, &env, scope, scope_names, arg_names);
                    context.define(
                        Option::Some(name.clone()),
                        scope_names
                            .iter()
                            .chain(arg_names.iter())
                            .cloned()
                            .collect(),
                        scope.clone(),
                        scope_names.to_vec(),
                        bir_value,
                    );
                    env.bind(
                        name.clone(),
                        Binding::LetSet {
                            name: name.clone(),
                            scope_names: scope_names.to_vec(),
                        },
                    );
                }
                body.bir_into(context, &env, scope, scope_names, arg_names)
            }
            If(cond, if_true, if_false) => {
                let bir_cond = cond.bir_into(context, env, scope, scope_names, arg_names);
                let bir_if_true = {
                    // can only reach if_true if cond
                    let scope =
                        context.narrow_scope(scope.clone(), scope_names.to_vec(), bir_cond.clone());
                    if_true.bir_into(context, env, &scope, scope_names, arg_names)
                };
                let bir_if_false = {
                    // can only reach if_false if !cond
                    let scope = context.narrow_scope(
                        scope.clone(),
                        scope_names.to_vec(),
                        B::Negate(box bir_cond.clone()),
                    );
                    if_false.bir_into(context, env, &scope, scope_names, arg_names)
                };
                B::If(box bir_cond, box bir_if_true, box bir_if_false)
            }
            Abstract(name, body) => {
                let mut env = env.clone();
                env.bind(name.clone(), Binding::Abstract);
                let new_scope_names = scope_names
                    .iter()
                    .chain(std::iter::once(&arg_names[0]))
                    .cloned()
                    .collect::<Vec<_>>();
                let scope = context.define(
                    Option::None,
                    new_scope_names.clone(),
                    scope.clone(),
                    scope_names.to_vec(),
                    B::ScalarEqual(
                        ScalarRef::Name(name.clone()),
                        ScalarRef::Name(arg_names[0].clone()),
                    ),
                );
                B::Intersect(
                    box B::ScalarEqual(
                        ScalarRef::Name(name.clone()),
                        ScalarRef::Name(arg_names[0].clone()),
                    ),
                    box body.bir_into(context, &env, &scope, &new_scope_names, &arg_names[1..]),
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
                let extra_arg_names = context.gensym.names(min_arity);
                let mut a_arg_names = arg_names.to_vec();
                let mut b_arg_names = arg_names.to_vec();
                if a_arity > b_arity {
                    a_arg_names.extend(extra_arg_names);
                } else {
                    b_arg_names.extend(extra_arg_names);
                }
                let bir_a = a.bir_into(context, env, scope, scope_names, &a_arg_names);
                // can only reach b if exists a
                let scope = context.narrow_scope(
                    scope.clone(),
                    scope_names.to_vec(),
                    B::Exists(a_arg_names, box bir_a.clone()),
                );
                let bir_b = b.bir_into(context, env, &scope, scope_names, &b_arg_names);
                B::Intersect(box bir_a, box bir_b)
            }
            Native(native) => B::Apply(
                ValueRef::Native(native.clone()),
                arg_names
                    .iter()
                    .map(|name| ScalarRef::Name(name.clone()))
                    .collect(),
            ),
            Reduce(..) | Seal(..) | Unseal(..) | Solve(..) => panic!("Unimplemented: {:?}", self),
        }
    }
}
