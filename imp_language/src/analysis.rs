use super::*;

#[derive(Debug, Clone)]
pub struct Cache<T> {
    cache: HashMap<*const Expression, T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarType {
    Any,
}

#[derive(Debug, Clone)]
pub enum ValueType {
    Nothing,
    Something,
    Product(ScalarType, Box<ValueType>), // no Abstract inside Product
    Abstract(ScalarType, Box<ValueType>),
}

#[derive(Debug, Clone)]
pub enum Arity {
    Exactly(usize),
    AtLeast(usize),
}

impl<T> Cache<T> {
    pub fn new() -> Self {
        Cache {
            cache: HashMap::new(),
        }
    }

    fn insert(&mut self, expr: &Expression, value: T) {
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

    fn product(self, tail: ValueType) -> ValueType {
        if tail.is_function() {
            ValueType::Abstract(self, box tail)
        } else {
            ValueType::Product(self, box tail)
        }
    }

    fn abstract_(self, tail: ValueType) -> ValueType {
        ValueType::Abstract(self, box tail)
    }
}

impl ValueType {
    pub fn is_function(&self) -> bool {
        match self {
            ValueType::Abstract(..) => true,
            _ => false,
        }
    }

    pub fn arity(&self) -> Arity {
        match self {
            ValueType::Nothing => Arity::AtLeast(0),
            ValueType::Something => Arity::Exactly(0),
            ValueType::Product(_, tail) | ValueType::Abstract(_, tail) => match tail.arity() {
                Arity::AtLeast(a) => Arity::AtLeast(a + 1),
                Arity::Exactly(a) => Arity::Exactly(a + 1),
            },
        }
    }

    fn union(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match (self, other) {
            (Nothing, t) | (t, Nothing) => t,
            (Something, Something) => Something,
            (Product(s1, t1), Product(s2, t2)) => Product(s1.union(s2)?, box t1.union(*t2)?),
            (Abstract(s1, t1), Abstract(s2, t2))
            | (Abstract(s1, t1), Product(s2, t2))
            | (Product(s1, t1), Abstract(s2, t2)) => Abstract(s1.union(s2)?, box t1.union(*t2)?),
            (t1, t2) => return Err(format!("Can't unify {:?} and {:?}", t1, t2)),
        })
    }

    fn intersect(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match (self, other) {
            (Nothing, _) | (_, Nothing) => Nothing,
            (Something, Something) => Something,
            (Product(s1, t1), Product(s2, t2)) => {
                Product(s1.intersect(s2)?, box t1.intersect(*t2)?)
            }
            (Abstract(s1, t1), Abstract(s2, t2))
            | (Abstract(s1, t1), Product(s2, t2))
            | (Product(s1, t1), Abstract(s2, t2)) => {
                Abstract(s1.intersect(s2)?, box t1.intersect(*t2)?)
            }
            (t1, t2) => return Err(format!("Can't unify {:?} and {:?}", t1, t2)),
        })
    }

    fn product(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match self {
            Nothing => Nothing,
            Something => match other {
                Abstract(..) => return Err(format!("Product of function: {:?}", other)),
                _ => other,
            },
            Product(s1, t1) => ValueType::Product(s1, box t1.product(other)?),
            Abstract(..) => return Err(format!("Product of function: {:?}", self)),
        })
    }

    fn apply(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match (self, other) {
            (Nothing, _) | (_, Nothing) => Nothing,
            (Something, t) | (t, Something) => t,
            (Product(s1, t1), Product(s2, t2))
            | (Abstract(s1, t1), Product(s2, t2))
            | (Product(s1, t1), Abstract(s2, t2)) => {
                // TODO intersect is fine for now, but may want to think more carefully about this once we have real scalar types
                s1.intersect(s2)?;
                t1.apply(*t2)?
            }
            (t1 @ Abstract(..), t2 @ Abstract(..)) => {
                return Err(format!("Applied function to function: {:?} {:?}", t1, t2));
            }
        })
    }

    fn negate(self) -> Self {
        use ValueType::*;
        match self {
            Nothing | Something => Something, // the type Something includes the values something and nothing. kinda weird
            Product(s, t) | Abstract(s, t) => Abstract(s, box t.negate()),
        }
    }

    fn solve(self) -> Self {
        use ValueType::*;
        match self {
            Nothing => Nothing,
            Something => Something,
            Product(s, t) | Abstract(s, t) => Product(s, box t.solve()),
        }
    }
}

impl Value {
    fn typ(&self) -> Result<ValueType, String> {
        // TODO this method is fishy - should seal capture checked types instead? might mess with staging
        use Value::*;
        Ok(match self {
            Set(set) => {
                let mut arities = set
                    .iter()
                    .map(|row| row.len())
                    .collect::<HashSet<_>>()
                    .into_iter();
                match (arities.next(), arities.next()) {
                    (None, None) => ValueType::Nothing,
                    (Some(arity), None) => {
                        let mut typ = ValueType::Something;
                        for _ in 0..arity {
                            typ = ValueType::Product(ScalarType::Any, box ValueType::Something);
                        }
                        typ
                    }
                    (_, Some(_)) => return Err(format!("Mismatched arities in: {}", self)),
                }
            }
            Closure(name, body, env) => {
                let mut type_env = Environment::from(
                    env.bindings
                        .iter()
                        .map(|(name, value)| Ok((name.clone(), value.typ()?)))
                        .collect::<Result<Vec<(String, ValueType)>, String>>()?,
                );
                type_env.bind(
                    name.clone(),
                    ValueType::Product(ScalarType::Any, box ValueType::Something),
                );
                let mut type_cache = Cache::new();
                body.typecheck(&type_env, &mut type_cache)?
            }
        })
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
            Exists(names, box body) => {
                let mut env = env.clone();
                for name in names {
                    env.bind(name.clone(), true);
                }
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
            Nothing => ValueType::Nothing,
            Something => ValueType::Something,
            Scalar(_) => ValueType::Product(ScalarType::Any, box ValueType::Something),
            Union(e1, e2) => e1.typecheck(env, cache)?.union(e2.typecheck(env, cache)?)?,
            Intersect(e1, e2) => e1
                .typecheck(env, cache)?
                .intersect(e2.typecheck(env, cache)?)?,
            Product(e1, e2) => e1
                .typecheck(env, cache)?
                .product(e2.typecheck(env, cache)?)?,
            Equal(e1, e2) => {
                // TODO intersect is fine for now, but may want to think more carefully about this once we have real scalar types
                e1.typecheck(env, cache)?
                    .intersect(e2.typecheck(env, cache)?)?;
                ValueType::Something
            }
            Negate(e) => e.typecheck(env, cache)?.negate(),
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
                    ValueType::Nothing | ValueType::Something => (),
                    other => return Err(format!("Non-boolean condition in `if`: {:?}", other)),
                }
                t.typecheck(env, cache)?.union(f.typecheck(env, cache)?)?
            }
            Abstract(arg, body) => {
                let mut env = env.clone();
                env.bind(
                    arg.clone(),
                    ValueType::Product(ScalarType::Any, box ValueType::Something),
                );
                ValueType::Abstract(ScalarType::Any, box body.typecheck(&env, cache)?)
            }
            Apply(e1, e2) => e1.typecheck(env, cache)?.apply(e2.typecheck(env, cache)?)?,
            ApplyNative(f, args) => {
                assert_eq!(f.input_arity, args.len());
                let mut t = ValueType::Something;
                for _ in 0..f.output_arity {
                    t = ValueType::Product(ScalarType::Any, box t)
                }
                t
            }
            Seal(e) => {
                e.typecheck(env, cache)?;
                ValueType::Product(ScalarType::Any, box ValueType::Something)
            }
            Unseal(..) => return Err(format!("Can't type unseal")),
            Exists(..) => return Err(format!("Can't type exists")),
            Solve(e) => e.typecheck(env, cache)?.solve(),
        };
        cache.insert(self, typ.clone());
        Ok(typ)
    }
}
