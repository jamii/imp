use crate::shared::*;

#[derive(Debug, Clone)]
pub struct ContainsContext<'a> {
    pub scalar_cache: &'a Cache<bool>,
    pub type_cache: &'a Cache<ValueType>,
    pub gensym: &'a Gensym,
}

impl Expression {
    pub fn lower(&self, context: &ContainsContext) -> Result<Self, String> {
        if *context.scalar_cache.get(self) {
            Ok(self.clone())
        } else {
            if let Expression::None = self {
                Ok(Expression::None)
            } else {
                let arity = context.type_cache.get(self).arity();
                let names = context.gensym.names(arity);
                let args = names
                    .iter()
                    .map(|name| Expression::Name(name.clone()))
                    .collect::<Vec<_>>();
                Ok(Expression::_abstract(names, self.contains(&args, context)?))
            }
        }
    }

    fn contains(&self, args: &[Expression], context: &ContainsContext) -> Result<Self, String> {
        use Expression::*;
        Ok(match self {
            None => None,
            Some => {
                assert_eq!(args.len(), 0);
                Some
            }
            Scalar(scalar) => {
                assert_eq!(args.len(), 1);
                Apply(box Scalar(scalar.clone()), box args[0].clone())
            }
            Union(box e1, box e2) => Union(
                box e1.contains(args, context)?,
                box e2.contains(args, context)?,
            ),
            Intersect(box e1, box e2) => Intersect(
                box e1.contains(args, context)?,
                box e2.contains(args, context)?,
            ),
            Product(box e1, box e2) => {
                let a1 = context.type_cache.get(e1).arity();
                Intersect(
                    box e1.contains(&args[0..a1], context)?,
                    box e2.contains(&args[a1..], context)?,
                )
            }
            Equal(box left, box right) => {
                assert_eq!(args.len(), 0);
                assert_eq!(args.len(), 0);
                match (
                    *context.scalar_cache.get(left),
                    *context.scalar_cache.get(right),
                ) {
                    (true, true) => Apply(box left.clone(), box right.clone()),
                    _ => Equal(box left.lower(context)?, box right.lower(context)?),
                }
            }
            Negate(box e) => {
                assert_eq!(args.len(), 0);
                Negate(box e.lower(context)?)
            }
            Name(name) => Expression::apply(Name(name.clone()), args.to_vec()),
            If(box cond, box if_true, box if_false) => If(
                box cond.contains(&[], context)?,
                box if_true.contains(args, context)?,
                box if_false.contains(args, context)?,
            ),
            Abstract(arg, box body) => {
                assert!(args.len() >= 1);
                // TODO can rename break if arg is not unique?
                body.contains(&args[1..], context)?.rename(arg, &args[0])
            }
            Apply(box left, box right) => {
                match (
                    *context.scalar_cache.get(left),
                    *context.scalar_cache.get(right),
                ) {
                    (true, true) => {
                        assert_eq!(args.len(), 0);
                        Apply(box left.clone(), box right.clone())
                    }
                    (true, false) => {
                        let mut args = args.to_vec();
                        args.insert(0, left.clone());
                        right.contains(&args, context)?
                    }
                    (false, true) => {
                        let mut args = args.to_vec();
                        args.insert(0, right.clone());
                        left.contains(&args, context)?
                    }
                    (false, false) => {
                        let left_arity = context.type_cache.get(left).arity();
                        let right_arity = context.type_cache.get(right).arity();
                        let mut left_args = context.gensym.name_exprs(left_arity.min(right_arity));
                        let mut right_args = left_args.clone();
                        if left_arity < right_arity {
                            right_args.extend_from_slice(args);
                        } else {
                            left_args.extend_from_slice(args);
                        }
                        Intersect(
                            box left.contains(&left_args, context)?,
                            box right.contains(&right_args, context)?,
                        )
                    }
                }
            }
            Native(native) => Expression::apply(Native(native.clone()), args.to_vec()),
            Solve(e) => e.contains(args, context)?,
            _ => return Err(format!("Can't lower {}", self)),
        })
    }
}
