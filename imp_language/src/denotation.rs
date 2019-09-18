use crate::shared::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scalar {
    String(String),
    Number(i64),
    Sealed(Box<Value>),
}

pub type Set = BTreeSet<Vec<Scalar>>;

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
}

impl Value {
    pub fn unseal(val: Value) -> Result<Value, String> {
        match val.as_scalar()? {
            Scalar::Sealed(box sealed) => Ok(sealed),
            _ => return Err(format!("${}", val)),
        }
    }

    pub fn is_nothing(&self) -> bool {
        match self {
            Value::Set(set) if set.len() == 0 => true,
            _ => false,
        }
    }

    pub fn is_something(&self) -> bool {
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

    pub fn nothing() -> Value {
        Value::set(vec![])
    }

    pub fn something() -> Value {
        Value::set(vec![vec![]])
    }

    pub fn scalar(scalar: Scalar) -> Value {
        Value::set(vec![vec![scalar]])
    }

    fn union(val1: Value, val2: Value) -> Result<Value, String> {
        use Expression::*;
        use Value::*;
        Ok(if val1.is_nothing() {
            val2
        } else if val2.is_nothing() {
            val1
        } else {
            match (val1, val2) {
                (Set(set1), Set(set2)) => Value::set(set1.union(&set2).cloned()),
                (v1, v2) => {
                    // (f | g) => (a -> (f a) | (g a))
                    let env = Environment::from(vec![("v1".to_owned(), v1), ("v2".to_owned(), v2)]);
                    let expr = Union(
                        box Apply(box Name("v1".to_owned()), box Name("a".to_owned())),
                        box Apply(box Name("v2".to_owned()), box Name("a".to_owned())),
                    );
                    Closure("a".to_owned(), expr, env)
                }
            }
        })
    }

    fn intersect(val1: Value, val2: Value) -> Result<Value, String> {
        use Expression::*;
        use Value::*;
        Ok(if val1.is_nothing() {
            Value::nothing()
        } else if val2.is_nothing() {
            Value::nothing()
        } else {
            match (val1, val2) {
                (Set(set1), Set(set2)) => Value::set(set1.intersection(&set2).cloned()),
                (v1, v2) => {
                    // (f & g) => (a -> (f a) & (g a))
                    let env = Environment::from(vec![("v1".to_owned(), v1), ("v2".to_owned(), v2)]);
                    let expr = Intersect(
                        box Apply(box Name("v1".to_owned()), box Name("a".to_owned())),
                        box Apply(box Name("v2".to_owned()), box Name("a".to_owned())),
                    );
                    Closure("a".to_owned(), expr, env)
                }
            }
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
            (v1, v2) => Err(format!("Type error: {} , {}", v1, v2))?,
        })
    }

    fn equals(val1: Value, val2: Value) -> Value {
        if val1 == val2 {
            Value::something()
        } else {
            Value::nothing()
        }
    }

    fn negate(val: Value) -> Value {
        use Expression::*;
        use Value::*;
        if val.is_nothing() {
            Value::something()
        } else if val.is_something() {
            Value::nothing()
        } else {
            // !f => (a -> !(f a))
            Closure(
                "a".to_owned(),
                Negate(box Apply(
                    box Name("v".to_owned()),
                    box Name("a".to_owned()),
                )),
                Environment::from(vec![("v".to_owned(), val)]),
            )
        }
    }

    fn apply(val1: Value, val2: Value) -> Result<Value, String> {
        use Value::*;
        Ok(match (val1, val2) {
            (Set(set1), Set(set2)) => {
                let mut tuples = vec![];
                for tuple1 in &set1 {
                    for tuple2 in &set2 {
                        if tuple2.starts_with(&tuple1) {
                            tuples.push(tuple2[tuple1.len()..].to_vec());
                        } else if tuple1.starts_with(&tuple2) {
                            tuples.push(tuple1[tuple2.len()..].to_vec());
                        }
                    }
                }
                Value::set(tuples)
            }
            (Closure(name, body, env), Set(set)) | (Set(set), Closure(name, body, env)) => {
                let mut set_iter = set.into_iter();
                match set_iter.next() {
                    None => Value::nothing(),
                    Some(tuple) => {
                        // fun (scalar x tuple | tail) => (fun scalar tuple | fun tail)
                        let head = {
                            let mut tuple_iter = tuple.into_iter();
                            match tuple_iter.next() {
                                None => Closure(name.clone(), body.clone(), env.clone()),
                                Some(scalar) => {
                                    let tuple = tuple_iter.collect::<Vec<_>>();
                                    let mut new_env = env.clone();
                                    new_env.bind(name.clone(), Value::scalar(scalar));
                                    Value::apply(
                                        body.clone().eval(&new_env)?,
                                        Value::set(vec![tuple]),
                                    )?
                                }
                            }
                        };
                        let tail = Value::apply(Closure(name, body, env), Value::set(set_iter))?;
                        Value::union(head, tail)?
                    }
                }
            }
            (v1, v2) => Err(format!("Type error: {} {}", v1, v2))?,
        })
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
    pub fn close_over(&self, names: BTreeSet<Name>) -> Self {
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
    pub fn free_names(&self) -> BTreeSet<Name> {
        fn visit(expr: &Expression, free_names: &mut BTreeSet<Name>, bound: &BTreeSet<Name>) {
            match expr {
                Expression::Name(name) => {
                    if !bound.contains(name) {
                        free_names.insert(name.clone());
                    }
                }
                Expression::ApplyNative(_, args) => {
                    for arg in args {
                        if !bound.contains(arg) {
                            free_names.insert(arg.clone());
                        }
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
        let mut free_names = BTreeSet::new();
        visit(self, &mut free_names, &BTreeSet::new());
        free_names
    }

    pub fn eval(self, env: &Environment<Value>) -> Result<Value, String> {
        use Expression::*;
        use Value::*;
        Ok(match self {
            Nothing => Value::nothing(),
            Something => Value::something(),
            Scalar(scalar) => Value::scalar(scalar),
            Union(box e1, box e2) => Value::union(e1.eval(env)?, e2.eval(env)?)?,
            Intersect(box e1, box e2) => Value::intersect(e1.eval(env)?, e2.eval(env)?)?,
            Product(box e1, box e2) => Value::product(e1.eval(env)?, e2.eval(env)?)?,
            Equal(box e1, box e2) => Value::equals(e1.eval(env)?, e2.eval(env)?),
            Negate(box e) => Value::negate(e.eval(env)?),
            Name(name) => match env.lookup(&name) {
                Some(value) => value.clone(),
                None => Err(format!("Undefined: {}", name))?,
            },
            Let(name, box value, box body) => {
                let mut env = env.clone();
                env.bind(name, value.eval(&env)?);
                body.eval(&env)?
            }
            If(box cond, box if_true, box if_false) => {
                if !cond.eval(env)?.is_nothing() {
                    if_true.eval(env)?
                } else {
                    if_false.eval(env)?
                }
            }
            Abstract(arg, box body) => {
                let closure_env = env.close_over(body.free_names());
                Closure(arg, body, closure_env)
            }
            Apply(fun, arg) => Value::apply(fun.eval(env)?, arg.eval(env)?)?,
            ApplyNative(native, args) => (native.fun)(
                args.into_iter()
                    // with_natives wraps ApplyNative in a function so by this point we know all the args exist and are scalars
                    .map(|arg| env.lookup(&arg).unwrap().clone().as_scalar().unwrap())
                    .collect(),
            )?,
            Seal(e) => Value::scalar(self::Scalar::Sealed(box e.eval(env)?)),
            Unseal(e) => Value::unseal(e.eval(env)?)?,
            Exists(..) | Solve(..) => return Err(format!("Unimplemented: {}", self)),
        })
    }
}
