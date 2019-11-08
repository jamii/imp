use crate::shared::*;

#[derive(Debug, Clone)]
pub struct Bir {
    pub typ: ValueType,
    pub names: Vec<Name>,
    pub body: Box<BooleanBir>,
}

#[derive(Debug, Clone)]
pub enum BooleanBir {
    None,
    Some,
    Union(Vec<BooleanBir>),
    Intersect(Vec<BooleanBir>),
    ScalarEqual(ScalarBir, ScalarBir),
    Equal(Bir, Bir),
    Negate(Box<BooleanBir>),
    Exists(Bir),
    Let(Name, Bir, Box<BooleanBir>),
    If(Box<BooleanBir>, Box<BooleanBir>, Box<BooleanBir>),
    Apply(ValueBir, Vec<ScalarBir>),
}

#[derive(Debug, Clone)]
pub enum ValueBir {
    Name(Name),
    Native(Native),
}

#[derive(Debug, Clone)]
pub enum ScalarBir {
    Name(Name),
    Scalar(Scalar),
}

#[derive(Debug, Clone)]
pub struct BirContext<'a> {
    pub renames: Vec<(Name, ScalarBir)>,
    pub type_cache: &'a Cache<ValueType>,
    pub gensym: &'a Gensym,
}

impl BooleanBir {
    fn union(left: BooleanBir, right: BooleanBir) -> BooleanBir {
        let mut birs = vec![];
        if let BooleanBir::Union(left_birs) = left {
            birs.extend(left_birs);
        } else {
            birs.push(left);
        }
        if let BooleanBir::Union(right_birs) = right {
            birs.extend(right_birs);
        } else {
            birs.push(right);
        }
        BooleanBir::Union(birs)
    }

    fn intersect(left: BooleanBir, right: BooleanBir) -> BooleanBir {
        let mut birs = vec![];
        if let BooleanBir::Intersect(left_birs) = left {
            birs.extend(left_birs);
        } else {
            birs.push(left);
        }
        if let BooleanBir::Intersect(right_birs) = right {
            birs.extend(right_birs);
        } else {
            birs.push(right);
        }
        BooleanBir::Intersect(birs)
    }
}

impl BirContext<'_> {
    fn rename(&self, name: &Name) -> Option<ScalarBir> {
        self.renames
            .iter()
            .find(|(name2, _)| name == name2)
            .map(|(_, s)| s.clone())
    }
}

impl Expression {
    pub fn as_bir(&self, context: &mut BirContext) -> Result<Bir, String> {
        let typ = context.type_cache.get(self).clone();
        Ok(match typ.arity() {
            Option::None => Bir {
                typ,
                names: vec![],
                body: box BooleanBir::None,
            },
            Option::Some(arity) => {
                let names = context.gensym.names(arity);
                let body = self.as_boolean_bir(
                    &names
                        .iter()
                        .map(|name| ScalarBir::Name(name.clone()))
                        .collect::<Vec<_>>(),
                    context,
                )?;
                Bir {
                    typ,
                    names,
                    body: box body,
                }
            }
        })
    }

    fn as_scalar_bir(&self, context: &BirContext) -> Option<ScalarBir> {
        match self {
            Expression::Name(name) => context.rename(name),
            Expression::Scalar(scalar) => Option::Some(ScalarBir::Scalar(scalar.clone())),
            _ => Option::None,
        }
    }

    fn as_boolean_bir(
        &self,
        args: &[ScalarBir],
        context: &mut BirContext,
    ) -> Result<BooleanBir, String> {
        use Expression::*;
        match context.type_cache.get(self).arity() {
            Option::None => return Ok(BooleanBir::None),
            _ => (),
        }
        Ok(match self {
            None => BooleanBir::None,
            Some => {
                assert_eq!(args.len(), 0);
                BooleanBir::Some
            }
            Scalar(scalar) => {
                assert_eq!(args.len(), 1);
                BooleanBir::ScalarEqual(ScalarBir::Scalar(scalar.clone()), args[0].clone())
            }
            Union(box e1, box e2) => BooleanBir::union(
                e1.as_boolean_bir(args, context)?,
                e2.as_boolean_bir(args, context)?,
            ),
            Intersect(box e1, box e2) => BooleanBir::intersect(
                e1.as_boolean_bir(args, context)?,
                e2.as_boolean_bir(args, context)?,
            ),
            Product(box e1, box e2) => {
                // if a: none then self: none
                let a1 = context.type_cache.get(e1).arity().unwrap();
                BooleanBir::intersect(
                    e1.as_boolean_bir(&args[0..a1], context)?,
                    e2.as_boolean_bir(&args[a1..], context)?,
                )
            }
            Equal(box left, box right) => {
                assert_eq!(args.len(), 0);
                assert_eq!(args.len(), 0);
                match (left.as_scalar_bir(context), right.as_scalar_bir(context)) {
                    (Option::Some(left), Option::Some(right)) => {
                        BooleanBir::ScalarEqual(left, right)
                    }
                    _ => BooleanBir::Equal(left.as_bir(context)?, right.as_bir(context)?),
                }
            }
            Negate(box e) => {
                assert_eq!(args.len(), 0);
                BooleanBir::Negate(box BooleanBir::Exists(e.as_bir(context)?))
            }
            Name(name) => match context.rename(name) {
                Option::Some(scalar_bir) => {
                    assert_eq!(args.len(), 1);
                    BooleanBir::ScalarEqual(scalar_bir, args[0].clone())
                }
                Option::None => BooleanBir::Apply(ValueBir::Name(name.clone()), args.to_vec()),
            },
            Let(name, value, body) => BooleanBir::Let(
                name.clone(),
                value.as_bir(context)?,
                box body.as_boolean_bir(args, context)?,
            ),
            If(box cond, box if_true, box if_false) => BooleanBir::If(
                box cond.as_boolean_bir(&[], context)?,
                box if_true.as_boolean_bir(args, context)?,
                box if_false.as_boolean_bir(args, context)?,
            ),
            Abstract(arg, box body) => {
                assert!(args.len() >= 1);
                context.renames.push((arg.clone(), args[0].clone()));
                let bir = body.as_boolean_bir(&args[1..], context)?;
                context.renames.pop();
                bir
            }
            Apply(box left, box right) => {
                match (left.as_scalar_bir(context), right.as_scalar_bir(context)) {
                    (Option::Some(left), Option::Some(right)) => {
                        assert_eq!(args.len(), 0);
                        BooleanBir::ScalarEqual(left, right)
                    }
                    (Option::Some(left), Option::None) => {
                        let mut args = args.to_vec();
                        args.insert(0, left);
                        right.as_boolean_bir(&args, context)?
                    }
                    (Option::None, Option::Some(right)) => {
                        let mut args = args.to_vec();
                        args.insert(0, right);
                        left.as_boolean_bir(&args, context)?
                    }
                    (Option::None, Option::None) => {
                        // if left: none or right: none then self: none
                        let left_arity = context.type_cache.get(left).arity().unwrap();
                        let right_arity = context.type_cache.get(right).arity().unwrap();
                        let new_names = context.gensym.names(left_arity.min(right_arity));
                        let mut left_args = new_names
                            .iter()
                            .map(|name| ScalarBir::Name(name.clone()))
                            .collect::<Vec<_>>();
                        let mut right_args = left_args.clone();
                        if left_arity < right_arity {
                            right_args.extend_from_slice(args);
                        } else {
                            left_args.extend_from_slice(args);
                        }
                        BooleanBir::Exists(Bir {
                            typ: context.type_cache.get(self).clone(),
                            names: new_names,
                            body: box BooleanBir::intersect(
                                left.as_boolean_bir(&left_args, context)?,
                                right.as_boolean_bir(&right_args, context)?,
                            ),
                        })
                    }
                }
            }
            Native(native) => BooleanBir::Apply(ValueBir::Native(native.clone()), args.to_vec()),
            Reduce(..) | Seal(..) | Unseal(..) | Solve(..) => {
                Err(format!("No BIR for {:?}", self))?
            }
        })
    }
}
