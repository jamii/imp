use crate::shared::*;

#[derive(Debug, Clone)]
pub struct NamedValue {
    pub name: Name,
    pub value: Value,
}

impl PartialEq for NamedValue {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for NamedValue {}

impl PartialOrd for NamedValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

impl Ord for NamedValue {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scalar {
    String(String),
    Number(i64),
    Sealed(
        Environment<NamedValue>,
        Environment<Scalar>,
        Box<Expression>,
    ),
}

pub type Set = BTreeSet<Vec<Scalar>>;

// TODO remove eq/ord
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Set(Set),
    Closure(Name, Expression, Environment<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Environment<T> {
    pub bindings: Vec<(Name, T)>,
}

impl Scalar {
    pub fn as_integer(&self) -> Result<i64, String> {
        match self {
            Scalar::Number(i) => Ok(*i),
            _ => Err(format!("Not an integer: {}", self)),
        }
    }

    pub fn unseal(scalar: Self) -> Result<Value, String> {
        match scalar {
            Scalar::Sealed(value_env, scalar_env, box sealed) => {
                let mut env = Environment::new();
                for (name, named_value) in value_env.bindings.into_iter() {
                    env.bind(name, named_value.value)
                }
                for (name, scalar) in scalar_env.bindings.into_iter() {
                    env.bind(name, Value::scalar(scalar));
                }
                let value = sealed.eval(&env)?;
                Ok(value)
            }
            _ => return Err(format!("${}", scalar)),
        }
    }
}

impl Value {
    pub fn unseal(val: Value) -> Result<Value, String> {
        Scalar::unseal(val.as_scalar()?)
    }

    pub fn is_some(&self) -> bool {
        match self {
            Value::Set(set) if set.len() == 1 => set.iter().next().unwrap().is_empty(),
            _ => false,
        }
    }

    pub fn as_scalar(&self) -> Result<Scalar, String> {
        if let Value::Set(set) = self {
            if set.len() == 1 {
                let tuple = set.iter().next().unwrap();
                if tuple.len() == 1 {
                    let scalar = tuple.iter().next().unwrap();
                    return Ok(scalar.clone());
                }
            }
        }
        Err(format!("Not a scalar: {}", self))
    }

    pub fn set<T: IntoIterator<Item = Vec<Scalar>>>(tuples: T) -> Value {
        Value::Set(tuples.into_iter().collect())
    }

    pub fn none() -> Value {
        Value::set(vec![])
    }

    pub fn some() -> Value {
        Value::set(vec![vec![]])
    }

    pub fn scalar(scalar: Scalar) -> Value {
        Value::set(vec![vec![scalar]])
    }

    fn union(val1: Value, val2: Value) -> Result<Value, String> {
        use Value::*;
        Ok(match (val1, val2) {
            (Set(set1), Set(set2)) => Value::set(set1.union(&set2).cloned()),
            (val1, val2) => return Err(format!("Internal error: {:?} | {:?}", val1, val2)),
        })
    }

    fn intersect(val1: Value, val2: Value) -> Result<Value, String> {
        use Value::*;
        Ok(match (val1, val2) {
            (Set(set1), Set(set2)) => Value::set(set1.intersection(&set2).cloned()),
            (val1, val2) => return Err(format!("Internal error: {:?} & {:?}", val1, val2)),
        })
    }

    fn product(val1: Value, val2: Value) -> Result<Value, String> {
        use Value::*;
        Ok(match (val1, val2) {
            (Set(set1), Set(set2)) => Value::set(set1.iter().flat_map(|tuple1| {
                set2.iter().map(move |tuple2| {
                    let mut tuple = tuple1.clone();
                    tuple.extend_from_slice(tuple2);
                    tuple
                })
            })),
            (val1, val2) => return Err(format!("Internal error: {:?} x {:?}", val1, val2)),
        })
    }

    fn equals(val1: Value, val2: Value) -> Result<Value, String> {
        use Value::*;
        Ok(match (val1, val2) {
            (Set(set1), Set(set2)) => {
                if set1 == set2 {
                    Value::some()
                } else {
                    Value::none()
                }
            }
            (v1, v2) => return Err(format!("{} = {}", v1, v2)),
        })
    }

    fn negate(val: Value) -> Result<Value, String> {
        use Value::*;
        Ok(match val {
            Set(set) => {
                if set.is_empty() {
                    Value::some()
                } else {
                    Value::none()
                }
            }
            val => return Err(format!("Internal error: !{:?}", val)),
        })
    }

    fn apply(val1: Value, val2: Value) -> Result<Value, String> {
        use Value::*;
        Ok(match (val1, val2) {
            (fun, Set(set)) | (Set(set), fun) => {
                let mut result = crate::Set::new();
                for row in set {
                    let mut fun = fun.clone();
                    let mut row_iter = row.into_iter();
                    loop {
                        match fun {
                            Closure(name, body, mut env) => {
                                // thanks to funify, if fun is a closure we must have scalars remaining
                                let scalar = row_iter.next().unwrap();
                                env.bind(name, Value::scalar(scalar));
                                fun = body.eval(&env)?;
                            }
                            Set(set1) => {
                                let row2 = row_iter.collect::<Vec<_>>();
                                for row1 in set1 {
                                    if row2.starts_with(&row1) {
                                        result.insert(row2[row1.len()..].to_vec());
                                    } else if row1.starts_with(&row2) {
                                        result.insert(row1[row2.len()..].to_vec());
                                    }
                                }
                                break;
                            }
                        }
                    }
                }
                Set(result)
            }
            (v1, v2) => Err(format!("Cannot eval: {} {}", v1, v2))?,
        })
    }

    fn reduce(init: Value, vals: Value, fun: Value) -> Result<Value, String> {
        match vals {
            Value::Closure(..) => Err(format!("Reduce on non-finite value: {}", vals)),
            Value::Set(set) => {
                let mut rows = set.into_iter().collect::<Vec<_>>();
                rows.sort();
                let mut reduced = init;
                for row in rows {
                    match row.into_iter().last() {
                        None => return Err(format!("Reduce on zero-column row")),
                        Some(val) => {
                            let env = Environment::from(vec![
                                ("fun".to_owned(), fun.clone()),
                                ("reduced".to_owned(), reduced.clone()),
                                ("val".to_owned(), Value::scalar(val)),
                            ]);
                            reduced = Expression::Apply(
                                box Expression::Apply(
                                    box Expression::Name("fun".to_owned()),
                                    box Expression::Name("reduced".to_owned()),
                                ),
                                box Expression::Name("val".to_owned()),
                            )
                            .eval(&env)?;
                        }
                    }
                }
                Ok(reduced)
            }
        }
    }
}

impl<T> Environment<T> {
    pub fn new() -> Self {
        Environment { bindings: vec![] }
    }

    pub fn from(bindings: Vec<(Name, T)>) -> Self {
        Environment { bindings: bindings }
    }

    pub fn lookup(&self, name: &Name) -> Option<&T> {
        self.bindings
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, e)| e)
    }

    pub fn bind(&mut self, name: Name, value: T) {
        self.bindings.push((name, value));
    }
}

impl<T> Environment<T>
where
    T: Clone,
{
    pub fn close_over(&self, names: HashSet<Name>) -> Self {
        Environment::from(
            self.bindings
                .iter()
                .filter(|(name, _)| names.contains(name))
                .cloned()
                .collect(),
        )
    }
}

impl Expression {
    pub fn free_names(&self) -> HashSet<Name> {
        fn visit(expr: &Expression, free_names: &mut HashSet<Name>, bound: &HashSet<Name>) {
            match expr {
                Expression::Name(name) => {
                    if !bound.contains(name) {
                        free_names.insert(name.clone());
                    }
                }
                Expression::Abstract(name, body) => {
                    let mut bound = bound.clone();
                    bound.insert(name.clone());
                    visit(body, free_names, &bound);
                }
                _ => {
                    expr.visit1(&mut |e| {
                        visit(e, free_names, bound);
                        Ok(())
                    })
                    .unwrap();
                }
            }
        }
        let mut free_names = HashSet::new();
        visit(self, &mut free_names, &HashSet::new());
        free_names
    }

    pub fn eval(self, env: &Environment<Value>) -> Result<Value, String> {
        use Expression::*;
        use Value::*;
        Ok(match self {
            None => Value::none(),
            Some => Value::some(),
            Scalar(scalar) => Value::scalar(scalar),
            Union(box e1, box e2) => Value::union(e1.eval(env)?, e2.eval(env)?)?,
            Intersect(box e1, box e2) => Value::intersect(e1.eval(env)?, e2.eval(env)?)?,
            Product(box e1, box e2) => Value::product(e1.eval(env)?, e2.eval(env)?)?,
            Equal(box e1, box e2) => Value::equals(e1.eval(env)?, e2.eval(env)?)?,
            Negate(box e) => Value::negate(e.eval(env)?)?,
            Name(name) => match env.lookup(&name) {
                Option::Some(value) => value.clone(),
                Option::None => Err(format!("Undefined: {}", name))?,
            },
            Let(name, box value, box body) => {
                let mut env = env.clone();
                env.bind(name, value.eval(&env)?);
                body.eval(&env)?
            }
            If(box cond, box if_true, box if_false) => {
                if cond.eval(env)?.is_some() {
                    if_true.eval(env)?
                } else {
                    if_false.eval(env)?
                }
            }
            Abstract(arg, box body) => {
                let closure_env = env.close_over(body.free_names());
                Closure(arg, body, closure_env)
            }
            Native(_) => unreachable!(), // handled by case below
            Apply(box Native(native), arg) => {
                let mut result = crate::Set::new();
                let set1 = match arg.eval(env)? {
                    Set(set1) => set1,
                    _ => unreachable!(),
                };
                for row in set1 {
                    let set2 = match (native.fun)(row[..native.input_arity].to_vec())? {
                        Set(set2) => set2,
                        _ => unreachable!(),
                    };
                    for row2 in set2 {
                        let row1 = row[native.input_arity..].to_vec();
                        if row2.starts_with(&row1) {
                            result.insert(row2[row1.len()..].to_vec());
                        } else if row1.starts_with(&row2) {
                            result.insert(row1[row2.len()..].to_vec());
                        }
                    }
                }
                Set(result)
            }
            Apply(fun, arg) => Value::apply(fun.eval(env)?, arg.eval(env)?)?,
            Reduce(init, vals, fun) => {
                Value::reduce(init.eval(env)?, vals.eval(env)?, fun.eval(env)?)?
            }
            Seal(e) => {
                let mut value_env = Environment::new();
                let mut scalar_env = Environment::new();
                for name in e.free_names() {
                    let value = env.lookup(&name).unwrap();
                    // anything that isn't a scalar should be a top-level value, so safe to compare by name instead of value
                    // TODO this is a bit of a hack - prefer to figure this out statically
                    match value.as_scalar() {
                        Ok(scalar) => scalar_env.bind(name, scalar),
                        Err(_) => value_env.bind(
                            name.clone(),
                            NamedValue {
                                name,
                                value: value.clone(),
                            },
                        ),
                    }
                }
                Value::scalar(self::Scalar::Sealed(value_env, scalar_env, e))
            }
            Unseal(e) => Value::unseal(e.eval(env)?)?,
            Exists(..) | Solve(..) => return Err(format!("Unimplemented: {}", self)),
        })
    }
}
