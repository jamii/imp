use crate::shared::*;

type ValueName = Name; // name created by BooleanBir::Let
type ScalarName = Name; // name created by Bir

#[derive(Debug, Clone)]
pub struct Bir {
    pub typ: ValueType,
    pub names: Vec<ScalarName>,
    pub body: Box<BooleanBir>,
}

#[derive(Debug, Clone)]
pub enum BooleanBir {
    None,
    Some,
    Union(Box<BooleanBir>, Box<BooleanBir>),
    Intersect(Box<BooleanBir>, Box<BooleanBir>),
    ScalarEqual(ScalarBir, ScalarBir),
    Equal(Bir, Bir),
    Negate(Box<BooleanBir>),
    Exists(Bir),
    Let(ValueName, Bir, Box<BooleanBir>),
    // If(Box<BooleanBir>, Box<BooleanBir>, Box<BooleanBir>),
    Apply(ValueBir, Vec<ScalarBir>),
}

#[derive(Debug, Clone)]
pub enum ValueBir {
    Name(ValueName),
    Native(Native),
}

#[derive(Debug, Clone)]
pub enum ScalarBir {
    Name(ScalarName),
    Scalar(Scalar),
}

#[derive(Debug, Clone)]
pub struct BirContext<'a> {
    pub renames: Vec<(Name, ScalarBir)>,
    pub type_cache: &'a Cache<ValueType>,
    pub gensym: &'a Gensym,
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
            Union(box e1, box e2) => BooleanBir::Union(
                box e1.as_boolean_bir(args, context)?,
                box e2.as_boolean_bir(args, context)?,
            ),
            Intersect(box e1, box e2) => BooleanBir::Intersect(
                box e1.as_boolean_bir(args, context)?,
                box e2.as_boolean_bir(args, context)?,
            ),
            Product(box e1, box e2) => {
                // if a: none then self: none
                let a1 = context.type_cache.get(e1).arity().unwrap();
                BooleanBir::Intersect(
                    box e1.as_boolean_bir(&args[0..a1], context)?,
                    box e2.as_boolean_bir(&args[a1..], context)?,
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
                match context.type_cache.get(e).arity() {
                    Option::None | Option::Some(0) => {
                        BooleanBir::Negate(box e.as_boolean_bir(&[], context)?)
                    }
                    _ => BooleanBir::Negate(box BooleanBir::Exists(e.as_bir(context)?)),
                }
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
            If(box cond, box if_true, box if_false) => {
                // let name = context.gensym.name();
                // let cond_bir = Bir {
                //     typ: context.type_cache.get(cond).clone(),
                //     names: vec![],
                //     body: box cond.as_boolean_bir(&[], context)?,
                // };
                // let cond = BooleanBir::Apply(ValueBir::Name(name.clone()), vec![]);
                // BooleanBir::Let(
                //     name,
                //     cond_bir,
                //     box BooleanBir::Union(
                //         box BooleanBir::Intersect(
                //             box cond.clone(),
                //             box if_true.as_boolean_bir(args, context)?,
                //         ),
                //         box BooleanBir::Intersect(
                //             box BooleanBir::Negate(box cond),
                //             box if_false.as_boolean_bir(args, context)?,
                //         ),
                //     ),
                // )

                let cond = cond.as_boolean_bir(&[], context)?;
                BooleanBir::Union(
                    box BooleanBir::Intersect(
                        box cond.clone(),
                        box if_true.as_boolean_bir(args, context)?,
                    ),
                    box BooleanBir::Intersect(
                        box BooleanBir::Negate(box cond),
                        box if_false.as_boolean_bir(args, context)?,
                    ),
                )
            }
            Abstract(arg, box body) => {
                assert!(args.len() >= 1);
                context.renames.push((arg.clone(), args[0].clone()));
                let bir = body.as_boolean_bir(&args[1..], context)?;
                context.renames.pop();
                bir
            }
            Apply(box left, box right) => {
                // match (left.as_scalar_bir(context), right.as_scalar_bir(context)) {
                //     (Option::Some(left), Option::Some(right)) => {
                //         assert_eq!(args.len(), 0);
                //         BooleanBir::ScalarEqual(left, right)
                //     }
                //     (Option::Some(left), Option::None) => {
                //         let mut args = args.to_vec();
                //         args.insert(0, left);
                //         right.as_boolean_bir(&args, context)?
                //     }
                //     (Option::None, Option::Some(right)) => {
                //         let mut args = args.to_vec();
                //         args.insert(0, right);
                //         left.as_boolean_bir(&args, context)?
                //     }
                //     (Option::None, Option::None) => {

                // if one side is a function, make sure we visit that side last
                let (left, right) = if context.type_cache.get(left).is_function() {
                    (right, left)
                } else {
                    (left, right)
                };

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
                    body: box BooleanBir::Intersect(
                        box left.as_boolean_bir(&left_args, context)?,
                        box right.as_boolean_bir(&right_args, context)?,
                    ),
                })
                // }
                // }
            }
            Native(native) => BooleanBir::Apply(ValueBir::Native(native.clone()), args.to_vec()),
            Reduce(..) | Seal(..) | Unseal(..) | Solve(..) => {
                Err(format!("No BIR for {:?}", self))?
            }
        })
    }
}

enum BirRefMut<'a> {
    Bir(&'a mut Bir),
    BooleanBir(&'a mut BooleanBir),
    ValueBir(&'a mut ValueBir),
    ScalarBir(&'a mut ScalarBir),
}

impl<'a> BirRefMut<'a> {
    fn visit_mut<F>(mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(BirRefMut) -> Result<(), String>,
    {
        use BirRefMut::*;
        match &mut self {
            Bir(b) => b.visit_mut(f)?,
            BooleanBir(b) => b.visit_mut(f)?,
            ValueBir(_) | ScalarBir(_) => (),
        }
        f(self)
    }
}

impl Bir {
    fn visit_mut<F>(&mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(BirRefMut) -> Result<(), String>,
    {
        self.body.visit_mut(f)?;
        f(BirRefMut::Bir(self))
    }
}

impl BooleanBir {
    fn visit1_mut<'a, F>(&'a mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(BirRefMut<'a>) -> Result<(), String>,
    {
        use BooleanBir::*;
        match self {
            None | Some => (),
            Union(b1, b2) | Intersect(b1, b2) => {
                f(BirRefMut::BooleanBir(b1))?;
                f(BirRefMut::BooleanBir(b2))?;
            }
            ScalarEqual(s1, s2) => {
                f(BirRefMut::ScalarBir(s1))?;
                f(BirRefMut::ScalarBir(s2))?;
            }
            Equal(b1, b2) => {
                f(BirRefMut::Bir(b1))?;
                f(BirRefMut::Bir(b2))?;
            }
            Negate(b) => f(BirRefMut::BooleanBir(b))?,
            Exists(b) => f(BirRefMut::Bir(b))?,
            Let(_, bv, bb) => {
                f(BirRefMut::Bir(bv))?;
                f(BirRefMut::BooleanBir(bb))?;
            }
            Apply(b, ss) => {
                f(BirRefMut::ValueBir(b))?;
                for s in ss {
                    f(BirRefMut::ScalarBir(s))?;
                }
            }
        }
        Ok(())
    }

    fn visit_mut<F>(&mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(BirRefMut) -> Result<(), String>,
    {
        self.visit1_mut(&mut |b| b.visit_mut(f))?;
        f(BirRefMut::BooleanBir(self))
    }

    // TODO doesn't need to be mutable
    pub fn free_scalar_names(&mut self) -> HashSet<Name> {
        let mut names = HashSet::new();
        self.visit_mut(&mut |bir| {
            match bir {
                BirRefMut::ScalarBir(ScalarBir::Name(name)) => {
                    names.insert(name.clone());
                }
                BirRefMut::Bir(bir) => {
                    for name in &bir.names {
                        names.remove(name);
                    }
                }
                _ => (),
            }
            Ok(())
        })
        .unwrap();
        names
    }
}

impl BooleanBir {
    fn sink_negates(&self, negated: bool) -> BooleanBir {
        use BooleanBir::*;
        match self {
            None => {
                if negated {
                    Some
                } else {
                    None
                }
            }
            Some => {
                if negated {
                    None
                } else {
                    Some
                }
            }
            Union(b1, b2) => {
                if negated {
                    Intersect(box b1.sink_negates(negated), box b2.sink_negates(negated))
                } else {
                    Union(box b1.sink_negates(negated), box b2.sink_negates(negated))
                }
            }
            Intersect(b1, b2) => {
                if negated {
                    Union(box b1.sink_negates(negated), box b2.sink_negates(negated))
                } else {
                    Intersect(box b1.sink_negates(negated), box b2.sink_negates(negated))
                }
            }
            Negate(box b) => b.sink_negates(!negated),
            Let(name, value, body) => {
                let mut value = value.clone();
                *value.body = value.body.sink_negates(false);
                Let(name.clone(), value, box body.sink_negates(negated))
            }
            ScalarEqual(..) | Apply(..) => {
                if negated {
                    Negate(box self.clone())
                } else {
                    self.clone()
                }
            }
            Exists(b) => {
                let mut b = b.clone();
                *b.body = b.body.sink_negates(false);
                if negated {
                    Negate(box Exists(b))
                } else {
                    Exists(b)
                }
            }
            Equal(b1, b2) => {
                let mut b1 = b1.clone();
                *b1.body = b1.body.sink_negates(false);
                let mut b2 = b2.clone();
                *b2.body = b2.body.sink_negates(false);
                if negated {
                    Negate(box Equal(b1, b2))
                } else {
                    Equal(b1, b2)
                }
            }
        }
    }

    // after sink_negates
    fn lift_unions(mut self) -> BooleanBir {
        use BooleanBir::*;
        self.visit_mut(&mut |bir_ref| {
            match bir_ref {
                BirRefMut::BooleanBir(b) => {
                    *b = match std::mem::replace(b, None) {
                        Intersect(box Union(b1a, b1b), b2) => {
                            Union(box Intersect(b1a, b2.clone()), box Intersect(b1b, b2))
                        }
                        Intersect(b1, box Union(b2a, b2b)) => {
                            Union(box Intersect(b1.clone(), b2a), box Intersect(b1, b2b))
                        }
                        b => b,
                    };
                }
                _ => (),
            };
            Ok(())
        })
        .unwrap();
        self
    }

    fn merge_bool(mut self) -> BooleanBir {
        use BooleanBir::*;
        self.visit_mut(&mut |bir_ref| {
            match bir_ref {
                BirRefMut::BooleanBir(b) => {
                    *b = match std::mem::replace(b, None) {
                        Intersect(box None, _) | Intersect(_, box None) => None,
                        Intersect(box Some, box b) | Intersect(box b, box Some) => b,
                        Union(box None, box b) | Union(box b, box None) => b,
                        Union(box Some, box b) | Union(box b, box Some) => Some,
                        Negate(box None) => Some,
                        Negate(box Some) => None,
                        Exists(Bir { body: box None, .. }) => None,
                        Exists(Bir { body: box Some, .. }) => Some,
                        b => b,
                    }
                }
                _ => (),
            };
            Ok(())
        })
        .unwrap();
        self
    }
}

impl Bir {
    pub fn dnf(mut self) -> Bir {
        *self.body = self.body.sink_negates(false).lift_unions().merge_bool();
        self
    }
}
