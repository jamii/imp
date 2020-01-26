use crate::shared::*;

/// Evaluate expr over each row in scope
#[derive(Debug)]
pub struct Global {
    pub name: Name,
    pub scope: Option<(Name, Vec<Name>)>,
    pub expr: Expression,
}

#[derive(Debug)]
struct FlattenContext<'a> {
    globals: RefCell<&'a mut Vec<Global>>,
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

impl<'a> FlattenContext<'a> {
    fn push_global(
        &self,
        name: Option<Name>,
        scope: &Name,
        scope_names: &[Name],
        expr: Expression,
    ) -> Name {
        let name = name.unwrap_or_else(|| self.gensym.name());
        println!("\n{}\n{:?}, {:?} &\n{}", name, scope, scope_names, expr);
        self.globals.borrow_mut().push(Global {
            name: name.clone(),
            scope: Some((scope.clone(), scope_names.to_vec())),
            expr,
        });
        name
    }
}

impl Expression {
    pub fn flatten(&self, type_cache: &Cache<ValueType>, gensym: &Gensym) -> Vec<Global> {
        let mut globals = vec![];
        globals.push(Global {
            name: "scope0".to_owned(),
            scope: None,
            expr: Expression::Some,
        });
        let context = FlattenContext {
            globals: RefCell::new(&mut globals),
            gensym,
            type_cache,
        };
        let expr = self.flatten_into(&context, &Environment::new(), &"scope0".to_owned(), &[]);
        globals.push(Global {
            name: "".to_owned(),
            scope: None,
            expr,
        });
        globals
    }

    fn flatten_into(
        &self,
        context: &FlattenContext,
        env: &Bindings,
        scope: &Name,
        scope_names: &[Name],
    ) -> Expression {
        use Expression::*;
        dbg!(self);

        let self_type = context.type_cache.get(self);
        assert!(!self_type.is_function());

        // not clear on the best choice here - sharing reduces redundant computation but also reduces the reach of join planning
        let maybe_share = |expr: Expression| {
            expr
            // Expression::Name(context.push_global(None, scope, scope_names, expr))
        };

        let self_arity = self_type.arity();
        match self {
            _ if self_arity.is_none() => None,
            None | Some | Scalar(_) | Native(_) => self.clone(),
            Union(a, b) => {
                if self_arity.unwrap_or(0) == 0 {
                    let flat_a = maybe_share(a.flatten_into(context, env, scope, scope_names));
                    // only reach b if !a
                    let scope = context.push_global(
                        Option::None,
                        scope,
                        scope_names,
                        Negate(box flat_a.clone()),
                    );
                    let flat_b = b.flatten_into(context, env, &scope, scope_names);
                    Product(box flat_a, box flat_b)
                } else {
                    let flat_a = a.flatten_into(context, env, scope, scope_names);
                    let flat_b = b.flatten_into(context, env, scope, scope_names);
                    Product(box flat_a, box flat_b)
                }
            }
            Intersect(a, b) => {
                let flat_a = maybe_share(a.flatten_into(context, env, scope, scope_names));
                // only reach b if !!a
                let scope = context.push_global(
                    Option::None,
                    scope,
                    scope_names,
                    Negate(box Negate(box flat_a.clone())),
                );
                let flat_b = b.flatten_into(context, env, &scope, scope_names);
                Intersect(box flat_a, box flat_b)
            }
            Product(a, b) => {
                let flat_a = maybe_share(a.flatten_into(context, env, scope, scope_names));
                // only reach b if !!a
                let scope = context.push_global(
                    Option::None,
                    scope,
                    scope_names,
                    Negate(box Negate(box flat_a.clone())),
                );
                let flat_b = b.flatten_into(context, env, &scope, scope_names);
                Product(box flat_a, box flat_b)
            }
            Equal(a, b) => Equal(
                box a.flatten_into(context, env, scope, scope_names),
                box b.flatten_into(context, env, scope, scope_names),
            ),
            Negate(a) => Negate(box a.flatten_into(context, env, scope, scope_names)),
            Name(name) => {
                match env.lookup(name).unwrap() {
                    Binding::Abstract => {
                        // already in scope_names
                        assert!(scope_names.iter().find(|name2| name == *name2).is_some());
                        Name(name.clone())
                    }
                    Binding::LetFun {
                        expr,
                        bindings: fun_env,
                    } => {
                        // eval 'closure' in current scope
                        expr.flatten_into(context, fun_env, scope, scope_names)
                    }
                    Binding::LetSet {
                        name,
                        scope_names: set_scope_names,
                    } => {
                        // refer to precomputed set for the current scope
                        Apply(
                            box Name(name.clone()),
                            box Expression::_product(Some, set_scope_names.to_vec()),
                        )
                    }
                }
            }
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
                    body.flatten_into(context, &env, scope, scope_names)
                } else {
                    let flat_value = value.flatten_into(context, &env, scope, scope_names);
                    let name = context.push_global(
                        Option::Some(name.clone()),
                        scope,
                        scope_names,
                        flat_value,
                    );
                    env.bind(
                        name.clone(),
                        Binding::LetSet {
                            name: name,
                            scope_names: scope_names.to_vec(),
                        },
                    );
                    body.flatten_into(context, &env, scope, scope_names)
                }
            }
            If(cond, if_true, if_false) => {
                let flat_cond = maybe_share(cond.flatten_into(context, env, scope, scope_names));
                let flat_if_true = {
                    // only reach if_true if cond
                    let scope =
                        context.push_global(Option::None, scope, scope_names, flat_cond.clone());
                    if_true.flatten_into(context, env, &scope, scope_names)
                };
                let flat_if_false = {
                    // only reach if_false if !cond
                    let scope = context.push_global(
                        Option::None,
                        scope,
                        scope_names,
                        Negate(box flat_cond.clone()),
                    );
                    if_false.flatten_into(context, env, &scope, scope_names)
                };
                If(box flat_cond, box flat_if_true, box flat_if_false)
            }
            Abstract(..) => {
                // self_type is finite
                unreachable!()
            }
            Apply(a, b) => {
                let mut a = &**a;
                let mut b = &**b;
                if context.type_cache.get(a).is_function() {
                    std::mem::swap(&mut a, &mut b);
                }
                let a_arity = context.type_cache.get(a).arity().unwrap_or(0);
                let b_fun_arity = context.type_cache.get(b).fun_arity().unwrap_or(0);
                assert!(a_arity >= b_fun_arity);
                if b_fun_arity == 0 {
                    let flat_a = a.flatten_into(context, &env, scope, scope_names);
                    let flat_b = b.flatten_into(context, &env, scope, scope_names);
                    Apply(box flat_a, box flat_b)
                } else if let Native(_) = b {
                    // already done
                    self.clone()
                } else {
                    let flat_a = maybe_share(a.flatten_into(context, &env, scope, scope_names));

                    // funified - should have enough names nearby to build the right scope
                    let mut b_names = vec![];
                    while let Abstract(name, new_b) = b {
                        b = &**new_b;
                        b_names.push(name.clone());
                    }
                    assert_eq!(b_fun_arity, b_names.len());

                    // currently have `a (b_names -> b)`
                    // want to have `a (b_names a_extra_names -> b a_extra_names)` so scope for b can be flat_a
                    let a_extra_names = context.gensym.names(a_arity - b_fun_arity);
                    let mut env = env.clone();
                    for name in b_names.iter().chain(a_extra_names.iter()) {
                        env.bind(name.clone(), Binding::Abstract)
                    }
                    let scope =
                        context.push_global(Option::None, scope, scope_names, flat_a.clone());
                    let scope_names = scope_names
                        .iter()
                        .chain(b_names.iter())
                        .chain(a_extra_names.iter())
                        .cloned()
                        .collect::<Vec<_>>();
                    let mut flat_b = b.flatten_into(context, &env, &scope, &scope_names);
                    flat_b = Apply(
                        box flat_b,
                        box Expression::_product(Some, a_extra_names.clone()),
                    );
                    for name in b_names.iter().chain(a_extra_names.iter()) {
                        flat_b = Abstract(name.clone(), box flat_b);
                    }
                    Apply(box flat_a, box flat_b)
                }
            }
            Reduce(..) | Seal(..) | Unseal(..) | Solve(..) => panic!("Unimplemented: {:?}", self),
        }
    }
}
