use crate::shared::*;

fn write_delimited<T, TS, F>(
    f: &mut fmt::Formatter,
    delimiter: &str,
    things: TS,
    write: F,
) -> fmt::Result
where
    TS: IntoIterator<Item = T>,
    F: Fn(&mut fmt::Formatter, T) -> fmt::Result,
{
    let mut iter = things.into_iter().peekable();
    while let Some(thing) = iter.next() {
        write(f, thing)?;
        if iter.peek().is_some() {
            f.write_str(delimiter)?;
        }
    }
    Ok(())
}

fn write_environment<T>(f: &mut fmt::Formatter, env: &Environment<T>) -> fmt::Result
where
    T: std::fmt::Display,
{
    for (var, value) in &env.bindings {
        write!(f, "let {} = {} in ", var, value)?;
    }
    Ok(())
}

impl fmt::Display for NamedValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
impl fmt::Display for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Scalar::String(string) => write!(f, "{:?}", string)?,
            Scalar::Number(number) => write!(f, "{:?}", number)?,
            // Scalar::Sealed(value_env, scalar_env, expr) => {
            //     write!(f, "{{")?;
            //     write_environment(f, value_env)?;
            //     write_environment(f, scalar_env)?;
            //     write!(f, "{}", expr)?;
            //     write!(f, "}}")?;
            // }
        }
        Ok(())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Set(set) => {
                if set.is_empty() {
                    write!(f, "none")?;
                } else if self.is_some() {
                    write!(f, "some")?;
                } else {
                    if !self.is_scalar() {
                        write!(f, "(")?;
                    }
                    write_delimited(f, " | ", set, |f, row| {
                        write_delimited(f, " x ", row, |f, scalar| write!(f, "{}", scalar))
                    })?;
                    if !self.is_scalar() {
                        write!(f, ")")?;
                    }
                }
            }
            Value::Closure(name, body, env) => {
                write!(f, "(")?;
                write_environment(f, env)?;
                write!(f, "{} -> {})", name, body)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;
        match self {
            None => write!(f, "none")?,
            Some => write!(f, "some")?,
            Scalar(scalar) => write!(f, "{}", scalar)?,
            Union(e1, e2) => write!(f, "({} | {})", e1, e2)?,
            Intersect(e1, e2) => write!(f, "({} & {})", e1, e2)?,
            Product(e1, e2) => write!(f, "({} x {})", e1, e2)?,
            Equal(e1, e2) => write!(f, "({} = {})", e1, e2)?,
            Negate(e) => write!(f, "!{}", e)?,
            Name(name) => write!(f, "{}", name)?,
            Let(name, value, body) => write!(f, "let {} = {} in {}", name, value, body)?,
            If(cond, if_true, if_false) => {
                write!(f, "if {} then {} else {}", cond, if_true, if_false)?
            }
            Abstract(arg, body) => {
                write!(f, "({} -> {})", arg, body)?;
            }
            Apply(fun, arg) => write!(f, "({} {})", fun, arg)?,
            Native(native) => write!(f, "<{}>", native.name)?,
            Reduce(init, vals, fun) => write!(f, "(reduce {} {} {})", init, vals, fun)?,
            Seal(e) => write!(f, "{{{}}}", e)?,
            Unseal(e) => write!(f, "${}", e)?,
            Solve(e) => {
                write!(f, "?({})", e)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ScalarType::*;
        match self {
            Any => write!(f, "any")?,
        }
        Ok(())
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValueType::*;
        match self {
            None => write!(f, "none")?,
            Maybe => write!(f, "maybe")?,
            Product(s, v) => write!(f, "{} x {}", s, v)?,
            Abstract(s, v) => write!(f, "{} -> {}", s, v)?,
        }
        Ok(())
    }
}

impl fmt::Display for Lirs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_delimited(f, "\n", &self.lirs, |f, lir| write!(f, "{}", lir))?;
        Ok(())
    }
}

impl fmt::Display for ValueLir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = ?(", self.name)?;
        write_delimited(f, " ", &self.args, |f, name| write!(f, "{}", name))?;
        write!(f, " -> {})", self.body)?;
        Ok(())
    }
}

impl fmt::Display for BooleanLir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BooleanLir::*;
        match self {
            None => write!(f, "none")?,
            Some => write!(f, "some")?,
            Union(b1, b2) => {
                write!(f, "({} | {})", b1, b2)?;
            }
            Intersect(b1, b2) => {
                write!(f, "({} & {})", b1, b2)?;
            }
            ScalarEqual(e1, e2) => write!(f, "({} == {})", e1, e2)?,
            Negate(e) => write!(f, "!{}", e)?,
            Apply(fun, args) => {
                write!(f, "({} ", fun)?;
                write_delimited(f, " ", args, |f, arg| write!(f, "{}", arg))?;
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValueRef::*;
        match self {
            Name(name) => write!(f, "{}", name)?,
            Native(native) => write!(f, "<{}>", native.name)?,
        }
        Ok(())
    }
}

impl fmt::Display for ScalarRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ScalarRef::*;
        match self {
            Name(name) => write!(f, "{}", name)?,
            Scalar(scalar) => write!(f, "{}", scalar)?,
        }
        Ok(())
    }
}

impl fmt::Display for Pirs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_delimited(f, "\n", self.pirs.iter().enumerate(), |f, (i, pir)| {
            write!(f, "{} = {:?}", i, pir)
        })?;
        Ok(())
    }
}
