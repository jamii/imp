use super::*;

#[derive(Debug, Clone)]
pub struct Cache<T> {
    cache: HashMap<*const Expression, T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarType {
    Any,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    None,
    Maybe,
    Product(ScalarType, Box<ValueType>), // no Abstract inside Product
    Abstract(ScalarType, Box<ValueType>),
}

impl<T> Cache<T> {
    pub fn new() -> Self {
        Cache {
            cache: HashMap::new(),
        }
    }

    pub fn insert(&mut self, expr: &Expression, value: T) {
        self.cache.insert(expr as *const Expression, value);
    }

    pub fn get(&self, expr: &Expression) -> &T {
        self.cache
            .get(&(expr as *const Expression))
            .unwrap_or_else(|| panic!("Not in cache: {:?}", expr))
    }
}

impl ScalarType {
    fn union(self, _other: Self) -> Result<Self, String> {
        Ok(ScalarType::Any)
    }

    fn intersect(self, _other: Self) -> Result<Self, String> {
        Ok(ScalarType::Any)
    }
}

impl ValueType {
    pub fn is_function(&self) -> bool {
        match self {
            ValueType::Abstract(..) => true,
            _ => false,
        }
    }

    pub fn arity(&self) -> Option<usize> {
        match self {
            ValueType::None => Option::None,
            ValueType::Maybe => Option::Some(0),
            ValueType::Product(_, tail) | ValueType::Abstract(_, tail) => {
                Option::Some(1 + tail.arity().unwrap())
            }
        }
    }

    pub fn fun_arity(&self) -> Option<usize> {
        match self {
            ValueType::None => Option::None,
            ValueType::Abstract(_, tail) => Option::Some(1 + tail.fun_arity().unwrap()),
            _ => Option::Some(0),
        }
    }

    fn union(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match (self, other) {
            (None, t) | (t, None) => t,
            (Maybe, Maybe) => Maybe,
            (Product(s1, t1), Product(s2, t2)) => Product(s1.union(s2)?, box t1.union(*t2)?),
            (Abstract(s1, t1), Abstract(s2, t2))
            | (Abstract(s1, t1), Product(s2, t2))
            | (Product(s1, t1), Abstract(s2, t2)) => Abstract(s1.union(s2)?, box t1.union(*t2)?),
            (t1, t2) => return Err(format!("Can't unify ({}) and ({})", t1, t2)),
        })
    }

    fn intersect(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match (self, other) {
            (None, _) | (_, None) => None,
            (Maybe, Maybe) => Maybe,
            (Product(s1, t1), Product(s2, t2)) => {
                Product(s1.intersect(s2)?, box t1.intersect(*t2)?)
            }
            (Abstract(s1, t1), Abstract(s2, t2))
            | (Abstract(s1, t1), Product(s2, t2))
            | (Product(s1, t1), Abstract(s2, t2)) => {
                Abstract(s1.intersect(s2)?, box t1.intersect(*t2)?)
            }
            (t1, t2) => return Err(format!("Can't unify ({}) and ({})", t1, t2)),
        })
    }

    fn product(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match self {
            None => None,
            Maybe => other,
            Product(s1, t1) => {
                let t2 = t1.product(other)?;
                match t2 {
                    None => None,
                    // surprising normalization rule!
                    Abstract(..) => Abstract(s1, box t2),
                    _ => Product(s1, box t2),
                }
            }
            Abstract(s1, t1) => {
                let t2 = t1.product(other)?;
                match t2 {
                    None => None,
                    _ => Abstract(s1, box t2),
                }
            }
        })
    }

    fn apply(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match (self, other) {
            (None, _) | (_, None) => None,
            (Maybe, t) | (t, Maybe) => t,
            (Product(s1, t1), Product(s2, t2))
            | (Abstract(s1, t1), Product(s2, t2))
            | (Product(s1, t1), Abstract(s2, t2)) => {
                // TODO intersect is fine for now, but may want to think more carefully about this once we have real scalar types
                s1.intersect(s2)?;
                t1.apply(*t2)?
            }
            (t1 @ Abstract(..), t2 @ Abstract(..)) => {
                return Err(format!("Applied function to function: ({}) ({})", t1, t2));
            }
        })
    }

    fn solve(self) -> Self {
        use ValueType::*;
        match self {
            None => None,
            Maybe => Maybe,
            Product(s, t) | Abstract(s, t) => Product(s, box t.solve()),
        }
    }
}

impl Expression {
    pub fn scalar(&self, env: &Environment<bool>, cache: &mut Cache<bool>) -> Result<bool, String> {
        use Expression::*;
        let scalar = match self {
            Scalar(_) => true,
            Name(name) => env
                .lookup(name)
                .ok_or_else(|| format!("Unbound name: {}", name))?
                .clone(),
            Let(name, box value, box body) => {
                let mut env = env.clone();
                env.bind(name.clone(), value.scalar(&env, cache)?);
                body.scalar(&env, cache)?
            }
            Abstract(arg, body) => {
                let mut env = env.clone();
                env.bind(arg.clone(), true);
                body.scalar(&env, cache)?
            }
            _ => {
                self.visit1(&mut |e| {
                    e.scalar(env, cache)?;
                    Ok(())
                })?;
                false
            }
        };
        cache.insert(self, scalar);
        Ok(scalar)
    }

    pub fn typecheck(
        &self,
        env: &Environment<ValueType>,
        cache: &mut Cache<ValueType>,
    ) -> Result<ValueType, String> {
        use Expression::*;
        let typ = match self {
            None => ValueType::None,
            Some => ValueType::Maybe,
            Scalar(_) => ValueType::Product(ScalarType::Any, box ValueType::Maybe),
            Union(e1, e2) => e1.typecheck(env, cache)?.union(e2.typecheck(env, cache)?)?,
            Intersect(e1, e2) => e1
                .typecheck(env, cache)?
                .intersect(e2.typecheck(env, cache)?)?,
            Product(e1, e2) => e1
                .typecheck(env, cache)?
                .product(e2.typecheck(env, cache)?)?,
            Negate(e) => match e.typecheck(env, cache)? {
                ValueType::Abstract(..) => return Err(format!("Negate on function: {}", e)),
                _ => ValueType::Maybe,
            },
            Equal(e1, e2) => {
                let t1 = e1.typecheck(env, cache)?;
                let t2 = e2.typecheck(env, cache)?;
                if t1.is_function() || t2.is_function() {
                    Err(format!("Equals on function: {} = {}", e1, e2))?
                }
                // TODO intersect is fine for now, but may want to think more carefully about this once we have real scalar types
                t1.intersect(t2)?;
                ValueType::Maybe
            }
            Name(name) => env
                .lookup(name)
                .ok_or_else(|| format!("Unbound variable: {:?}", name))?
                .clone(),
            Let(name, value, body) => {
                let mut env = env.clone();
                env.bind(name.clone(), value.typecheck(&env, cache)?);
                body.typecheck(&env, cache)?
            }
            If(c, t, f) => {
                match c.typecheck(env, cache)? {
                    ValueType::None | ValueType::Maybe => (),
                    other => return Err(format!("Non-boolean condition in `if`: {}", other)),
                }
                t.typecheck(env, cache)?.union(f.typecheck(env, cache)?)?
            }
            Abstract(arg, body) => {
                let mut env = env.clone();
                env.bind(
                    arg.clone(),
                    ValueType::Product(ScalarType::Any, box ValueType::Maybe),
                );
                let body_type = body.typecheck(&env, cache)?;
                match body_type {
                    ValueType::None => ValueType::None,
                    _ => ValueType::Abstract(ScalarType::Any, box body_type),
                }
            }
            Apply(e1, e2) => e1.typecheck(env, cache)?.apply(e2.typecheck(env, cache)?)?,
            Native(native) => {
                let mut t = ValueType::Maybe;
                for _ in 0..native.output_arity {
                    t = ValueType::Product(ScalarType::Any, box t)
                }
                for _ in 0..native.input_arity {
                    t = ValueType::Abstract(ScalarType::Any, box t)
                }
                t
            }
            Reduce(init, vals, fun) => {
                let init_type = init.typecheck(&env, cache)?;
                let vals_type = vals.typecheck(&env, cache)?;
                let fun_type = fun.typecheck(&env, cache)?;
                if vals_type.is_function() {
                    return Err(format!("Reduce on non-finite: {}", vals_type));
                }
                if let ValueType::Maybe = vals_type {
                    return Err(format!("Reduce on zero-column type"));
                }
                let reinit_type = fun_type
                    .apply(init_type.clone())?
                    .apply(ValueType::Product(ScalarType::Any, box ValueType::Maybe))?;
                // TODO do we need to fixpoint this?
                if let Err(_) = init_type.clone().union(reinit_type.clone()) {
                    return Err(format!(
                        "Function application with reduce has type {}, expected {}",
                        reinit_type, init_type
                    ));
                }
                init_type
            }
            Seal(e) => {
                e.typecheck(env, cache)?;
                ValueType::Product(ScalarType::Any, box ValueType::Maybe)
            }
            Unseal(..) => return Err(format!("Can't type unseal")),
            Solve(e) => e.typecheck(env, cache)?.solve(),
        };
        cache.insert(self, typ.clone());
        Ok(typ)
    }

    pub fn funify(&mut self, type_cache: &mut Cache<ValueType>, gensym: &Gensym) {
        use Expression::*;
        self.visit1_mut(&mut |expr| {
            expr.funify(type_cache, gensym);
            Ok(())
        })
        .unwrap();
        let typ = type_cache.get(&self);
        match typ.fun_arity() {
            Option::None => *self = None,
            Option::Some(fun_arity) => {
                if fun_arity > 0 {
                    let names = gensym.names(fun_arity);
                    *self = match self.take() {
                        Union(a, b) => Expression::_abstract(
                            names.clone(),
                            Union(
                                box Apply(a, box Expression::_product(Some, names.clone())),
                                box Apply(b, box Expression::_product(Some, names.clone())),
                            ),
                        ),
                        Intersect(a, b) => Expression::_abstract(
                            names.clone(),
                            Intersect(
                                box Apply(a, box Expression::_product(Some, names.clone())),
                                box Apply(b, box Expression::_product(Some, names.clone())),
                            ),
                        ),
                        Product(a, b) => {
                            // if a: none then self: none
                            let a_arity = type_cache.get(&a).arity().unwrap();
                            let split = fun_arity.min(a_arity);
                            Expression::_abstract(
                                names.clone(),
                                Product(
                                    box Apply(
                                        a,
                                        box Expression::_product(Some, names[0..split].to_vec()),
                                    ),
                                    box Apply(
                                        b,
                                        box Expression::_product(Some, names[split..].to_vec()),
                                    ),
                                ),
                            )
                        }
                        Apply(a, b) => {
                            let (a, b) = if type_cache.get(&a).is_function() {
                                (a, b)
                            } else {
                                (b, a)
                            };
                            Expression::_abstract(
                                names.clone(),
                                Apply(a, box Expression::_product(*b, names)),
                            )
                        }
                        Native(native) => Expression::_abstract(
                            names.clone(),
                            Apply(box Native(native), box Expression::_product(Some, names)),
                        ),
                        other => other,
                    }
                }
            }
        }
    }

    pub fn desugar(self, gensym: &Gensym) -> Self {
        use Expression::*;
        self.map(|expr| {
            Ok(if let If(cond, if_true, if_false) = expr {
                let cond_name = gensym.name();
                Let(
                    cond_name.clone(),
                    cond,
                    box Union(
                        box Product(box Name(cond_name.clone()), if_true),
                        box Product(box Negate(box Name(cond_name.clone())), if_false),
                    ),
                )
            } else {
                expr
            })
        })
        .unwrap()
    }

    // pub fn simplify(self, scalar_names: &HashSet<Name>) -> Self {
    //     use Expression::*;
    //     match self {
    //         Apply(box Abstract(name1, body), box Name(name2))
    //         | Apply(box Name(name2), box Abstract(name1, body))
    //             if scalar_names.contains(&name2) =>
    //         {
    //             body.rename(&name1, &Name(name2))
    //         }
    //         Apply(box Abstract(name1, body), box Scalar(scalar))
    //         | Apply(box Scalar(scalar), box Abstract(name1, body)) => {
    //             body.rename(&name1, &Scalar(scalar))
    //         }
    //         Abstract(name, body) => {
    //             let mut scalar_names = scalar_names.clone();
    //             scalar_names.insert(name.clone());
    //             Abstract(name, box body.simplify(&scalar_names))
    //         }
    //         other => other.map1(|expr| Ok(expr.simplify(scalar_names))).unwrap(),
    //     }
    // }
}
