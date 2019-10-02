use crate::shared::*;

pub type Name = String; // non-empty

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
// TODO Eq/Ord for fn are dubious - should implement on name instead
pub struct Native {
    pub name: Name,
    pub input_arity: usize,
    pub output_arity: usize,
    pub fun: fn(Vec<Scalar>) -> Result<Value, String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    Nothing,
    Something,
    Scalar(Scalar),
    Union(Box<Expression>, Box<Expression>),
    Intersect(Box<Expression>, Box<Expression>),
    Product(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    Negate(Box<Expression>),
    Name(Name),
    Let(Name, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Abstract(Name, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
    ApplyNative(Native, Vec<Name>),
    Reduce(Box<Expression>, Box<Expression>, Box<Expression>),
    Seal(Box<Expression>),
    Unseal(Box<Expression>),
    Exists(Vec<Name>, Box<Expression>),
    Solve(Box<Expression>),
}

impl Expression {
    pub fn _abstract(mut args: Vec<Name>, body: Expression) -> Expression {
        args.reverse();
        args.into_iter()
            .fold(body, |body, arg| Expression::Abstract(arg, box body))
    }

    pub fn apply(fun: &str, args: Vec<Expression>) -> Expression {
        args.into_iter()
            .fold(Expression::Name(fun.to_owned()), |fun, arg| {
                Expression::Apply(box fun, box arg)
            })
    }

    pub fn visit1<'a, F>(&'a self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&'a Expression) -> Result<(), String>,
    {
        use Expression::*;
        match self {
            Nothing => (),
            Something => (),
            Scalar(_) => (),
            Union(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Intersect(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Product(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Equal(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Negate(e) => f(&*e)?,
            Name(_) => (),
            Let(_, value, body) => {
                f(&*value)?;
                f(&*body)?;
            }
            If(cond, if_true, if_false) => {
                f(&*cond)?;
                f(&*if_true)?;
                f(&*if_false)?;
            }
            Abstract(_, body) => f(&*body)?,
            Apply(fun, arg) => {
                f(&*fun)?;
                f(&*arg)?;
            }
            ApplyNative(_, _) => (),
            Reduce(init, vals, fun) => {
                f(&*init)?;
                f(&*vals)?;
                f(&*fun)?;
            }
            Seal(e) => f(&*e)?,
            Unseal(e) => f(&*e)?,
            Exists(_, body) => f(&*body)?,
            Solve(e) => f(&*e)?,
        }
        Ok(())
    }

    pub fn visit<'a, F>(&'a self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&'a Expression) -> Result<(), String>,
    {
        self.visit1(&mut |e| e.visit(f))?;
        f(self)
    }

    pub fn visit1_mut<F>(&mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&mut Expression) -> Result<(), String>,
    {
        use Expression::*;
        match self {
            Nothing => (),
            Something => (),
            Scalar(_) => (),
            Union(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Intersect(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Product(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Equal(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Negate(box e) => f(e)?,
            Name(_) => (),
            Let(_, box value, box body) => {
                f(value)?;
                f(body)?;
            }
            If(box cond, box if_true, box if_false) => {
                f(cond)?;
                f(if_true)?;
                f(if_false)?;
            }
            Abstract(_, box body) => f(body)?,
            Apply(box fun, box arg) => {
                f(fun)?;
                f(arg)?;
            }
            ApplyNative(_, _) => (),
            Reduce(init, vals, fun) => {
                f(init)?;
                f(vals)?;
                f(fun)?;
            }
            Seal(box e) => f(e)?,
            Unseal(box e) => f(e)?,
            Exists(_, box body) => f(body)?,
            Solve(box e) => f(e)?,
        }
        Ok(())
    }

    pub fn visit_mut<F>(&mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&mut Expression) -> Result<(), String>,
    {
        self.visit1_mut(&mut |e| e.visit_mut(f))?;
        f(self)
    }

    pub fn map1<F>(mut self, mut f: F) -> Result<Self, String>
    where
        F: FnMut(Expression) -> Result<Expression, String>,
    {
        self.visit1_mut(&mut |e| {
            *e = f(std::mem::replace(e, Expression::Nothing))?;
            Ok(())
        })?;
        Ok(self)
    }

    pub fn map<F>(mut self, mut f: F) -> Result<Self, String>
    where
        F: FnMut(Expression) -> Result<Expression, String>,
    {
        self.visit_mut(&mut |e| {
            *e = f(std::mem::replace(e, Expression::Nothing))?;
            Ok(())
        })?;
        Ok(self)
    }

    pub fn with_natives(self, natives: &[Native]) -> Expression {
        fn map(
            expr: Expression,
            natives: &BTreeMap<Name, &Native>,
            bound: &BTreeSet<Name>,
        ) -> Expression {
            use Expression::*;
            match expr {
                Name(name) => {
                    if let (false, Some(native)) = (bound.contains(&name), natives.get(&name)) {
                        let args = (0..native.input_arity)
                            .map(|i| format!("a{}", i))
                            .collect::<Vec<_>>();
                        Expression::_abstract(args.clone(), ApplyNative((*native).clone(), args))
                    } else {
                        Name(name)
                    }
                }
                Abstract(arg, box body) => {
                    let mut bound = bound.clone();
                    bound.insert(arg.clone());
                    Abstract(arg, box map(body, natives, &bound))
                }
                expr => expr.map1(|e| Ok(map(e, natives, bound))).unwrap(),
            }
        }
        let natives = BTreeMap::from_iter(natives.into_iter().map(|n| (n.name.clone(), n)));
        map(self, &natives, &BTreeSet::new())
    }

    pub fn rename(self, old: &Name, new: &Name) -> Self {
        use Expression::*;
        match self {
            Name(name) => {
                if name == *old {
                    Name(new.clone())
                } else {
                    Name(name)
                }
            }
            Let(name, value, body) => {
                let body = if name == *old {
                    body
                } else {
                    box body.rename(old, new)
                };
                Let(name, box value.rename(old, new), body)
            }
            Abstract(arg, body) => {
                let body = if arg == *old {
                    body
                } else {
                    box body.rename(old, new)
                };
                Abstract(arg, body)
            }
            ApplyNative(f, mut args) => {
                for arg in &mut args {
                    if arg == old {
                        *arg = new.clone();
                    }
                }
                ApplyNative(f, args)
            }
            _ => self.map1(|expr| Ok(expr.rename(old, new))).unwrap(),
        }
    }

    pub fn with_unique_names(self) -> Result<Self, String> {
        fn map(
            expression: Expression,
            bound: &HashMap<Name, Name>,
            last_id: &mut HashMap<Name, usize>,
        ) -> Result<Expression, String> {
            use Expression::*;
            Ok(match expression {
                Name(name) => match bound.get(&name) {
                    Some(unique_name) => Name(unique_name.clone()),
                    None => return Err(format!("Undefined: {}", name)),
                },
                ApplyNative(native, names) => {
                    let names = names
                        .into_iter()
                        .map(|name| match bound.get(&name) {
                            Some(unique_name) => Ok(unique_name.clone()),
                            None => Err(format!("Undefined: {}", name)),
                        })
                        .collect::<Result<Vec<_>, String>>()?;
                    ApplyNative(native, names)
                }
                Let(name, value, body) => {
                    let value = map(*value, bound, last_id)?;
                    let id = last_id.entry(name.clone()).or_insert(0);
                    *id += 1;
                    let unique_name = format!("{}_{}", name, id);
                    let mut bound = bound.clone();
                    bound.insert(name, unique_name.clone());
                    let body = map(*body, &bound, last_id)?;
                    Let(unique_name, box value, box body)
                }
                Abstract(name, body) => {
                    let id = last_id.entry(name.clone()).or_insert(0);
                    *id += 1;
                    let unique_name = format!("{}_{}", name, id);
                    let mut bound = bound.clone();
                    bound.insert(name, unique_name.clone());
                    let body = map(*body, &bound, last_id)?;
                    Abstract(unique_name, box body)
                }
                _ => expression.map1(|e| map(e, bound, last_id))?,
            })
        }
        map(self, &HashMap::new(), &mut HashMap::new())
    }
}
