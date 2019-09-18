use crate::shared::*;

#[derive(Debug, Clone)]
enum LowerAction {
    Refer(String, Vec<Name>),
    Inline(Expression),
}

impl Expression {
    fn contains(
        &self,
        args: &[Name],
        actions: &Environment<LowerAction>,
        scalar_cache: &Cache<bool>,
        type_cache: &Cache<ValueType>,
        next_tmp: &mut usize,
        lets: &mut Vec<(Name, Vec<Name>, Expression)>,
    ) -> Result<Self, String> {
        use Expression::*;
        Ok(match self {
            Nothing => Nothing,
            Something => {
                assert_eq!(args.len(), 0);
                Something
            }
            Scalar(scalar) => {
                assert_eq!(args.len(), 1);
                Apply(box Scalar(scalar.clone()), box Name(args[0].clone()))
            }
            Union(box e1, box e2) => Union(
                box e1.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
                box e2.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
            ),
            Intersect(box e1, box e2) => Intersect(
                box e1.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
                box e2.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
            ),
            Product(box e1, box e2) => {
                let a1 = match type_cache.get(e1).arity() {
                    Arity::Exactly(a) => a,
                    Arity::AtLeast(_) => 0,
                };
                Intersect(
                    box e1.contains(
                        &args[0..a1],
                        actions,
                        scalar_cache,
                        type_cache,
                        next_tmp,
                        lets,
                    )?,
                    box e2.contains(
                        &args[a1..],
                        actions,
                        scalar_cache,
                        type_cache,
                        next_tmp,
                        lets,
                    )?,
                )
            }
            Equal(box e1, box e2) => {
                assert_eq!(args.len(), 0);
                Equal(box e1.clone(), box e2.clone())
            }
            Negate(box e) => {
                Negate(box e.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?)
            }
            Name(name) => match actions.lookup(name) {
                Some(LowerAction::Inline(expr)) => {
                    expr.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?
                }
                // TODO might need to be careful about shadowing here if variable names are not unique
                Some(LowerAction::Refer(new_name, prefix_args)) => {
                    // TODO this will have the wrong var names
                    let expr = Expression::apply(
                        new_name,
                        prefix_args.iter().map(|arg| Name(arg.clone())).collect(),
                    );
                    expr
                }
                None => Err(format!("Name from outside of lower {:?}", name))?,
            },
            Let(name, value, box body) => {
                let mut actions = actions.clone();
                match type_cache.get(&value) {
                    ValueType::Abstract(..) => {
                        actions.bind(name.clone(), LowerAction::Inline((**value).clone()));
                        body.contains(args, &actions, scalar_cache, type_cache, next_tmp, lets)?
                    }
                    typ => {
                        let arity = match typ.arity() {
                            Arity::Exactly(a) => a,
                            Arity::AtLeast(a) => a,
                        };
                        let value_args = (0..arity)
                            .map(|_| {
                                let name = format!("var{}", next_tmp);
                                *next_tmp += 1;
                                name
                            })
                            .collect::<Vec<_>>();
                        let value = value.contains(
                            &value_args,
                            &actions,
                            scalar_cache,
                            type_cache,
                            next_tmp,
                            lets,
                        )?;
                        let new_name = format!("{}_{}", name, next_tmp);
                        *next_tmp += 1;
                        lets.push((new_name.clone(), args.to_vec(), value));
                        actions.bind(name.clone(), LowerAction::Refer(new_name, args.to_vec()));
                        body.contains(args, &actions, scalar_cache, type_cache, next_tmp, lets)?
                    }
                }
            }
            If(box cond, box if_true, box if_false) => If(
                box cond.contains(&[], actions, scalar_cache, type_cache, next_tmp, lets)?,
                box if_true.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
                box if_false.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
            ),
            Abstract(arg, box body) => {
                assert!(args.len() >= 1); // TODO otherwise replace with nothing
                let expr = body
                    .contains(
                        &args[1..],
                        actions,
                        scalar_cache,
                        type_cache,
                        next_tmp,
                        lets,
                    )?
                    // TODO does this assume arg is unique?
                    .rename(arg, &args[0]);
                expr
            }
            Apply(box left, box right) => {
                match (*scalar_cache.get(left), *scalar_cache.get(right)) {
                    (true, true) => {
                        assert_eq!(args.len(), 0);
                        Equal(box left.clone(), box right.clone())
                    }
                    (true, false) => {
                        let mut args = args.to_vec();
                        let left_name = match left {
                            Name(name) => name,
                            _ => unreachable!(),
                        };
                        args.insert(0, left_name.clone());
                        right.contains(&args, actions, scalar_cache, type_cache, next_tmp, lets)?
                    }
                    (false, true) => {
                        let mut args = args.to_vec();
                        let right_name = match right {
                            Name(name) => name,
                            _ => unreachable!(),
                        };
                        args.insert(0, right_name.clone());
                        left.contains(&args, actions, scalar_cache, type_cache, next_tmp, lets)?
                    }
                    (false, false) => {
                        let left_arity = match type_cache.get(left).arity() {
                            Arity::Exactly(a) => a,
                            Arity::AtLeast(_) => 0,
                        };
                        let right_arity = match type_cache.get(right).arity() {
                            Arity::Exactly(a) => a,
                            Arity::AtLeast(_) => 0,
                        };
                        let new_arg_names = (0..left_arity.min(right_arity))
                            .map(|_| {
                                let name = format!("extra_var{}", next_tmp);
                                *next_tmp += 1;
                                name
                            })
                            .collect::<Vec<_>>();
                        let mut left_args = new_arg_names.clone();
                        let mut right_args = new_arg_names;
                        if left_arity < right_arity {
                            right_args.extend_from_slice(args);
                        } else {
                            left_args.extend_from_slice(args);
                        }
                        Intersect(
                            box left.contains(
                                &left_args,
                                actions,
                                scalar_cache,
                                type_cache,
                                next_tmp,
                                lets,
                            )?,
                            box right.contains(
                                &right_args,
                                actions,
                                scalar_cache,
                                type_cache,
                                next_tmp,
                                lets,
                            )?,
                        )
                    }
                }
            }
            ApplyNative(native, native_args) => {
                assert_eq!(args.len(), native.output_arity);
                ApplyNative(native.clone(), native_args.clone())
            }
            // Seal(e) => {
            //     assert_eq!(args.len(), 1);
            //     Apply(box Seal(e.clone()), box Name(args[0].clone()))
            // }
            Solve(e) => {
                // TODO check e is finite here (or elsewhere)
                e.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?
            }
            _ => return Err(format!("Can't lower {}", self)),
        })
    }

    pub fn lower(
        &self,
        scalar_cache: &Cache<bool>,
        type_cache: &Cache<ValueType>,
    ) -> Result<Vec<(Name, Vec<Name>, Expression)>, String> {
        let scope = Environment::new();
        let mut next_tmp = 0;
        let arity = match type_cache.get(self).arity() {
            Arity::Exactly(a) => a,
            Arity::AtLeast(_) => 0,
        };
        let args = (0..arity)
            .map(|_| {
                let name = format!("var{}", next_tmp);
                next_tmp += 1;
                name
            })
            .collect::<Vec<_>>();
        let mut lets = vec![];
        let predicate = self.contains(
            &args,
            &scope,
            scalar_cache,
            type_cache,
            &mut next_tmp,
            &mut lets,
        )?;
        lets.push((format!("result{}", next_tmp), args, predicate));
        Ok(lets)
    }

    // pub fn reorder(
    //     self,
    //     ordering: &[Expression],
    //     indexes: &mut Vec<(Name, Expression, Vec<usize>)>,
    //     next_tmp: &mut usize,
    // ) -> Expression {
    //     use Expression::*;
    //     if let Apply(..) = self {
    //         let mut f = self;
    //         let mut args: Vec<Expression> = vec![];
    //         while let Apply(box e1, box e2) = f {
    //             f = e1;
    //             args.insert(0, e2.clone());
    //         }
    //         let name = {
    //             let name = format!("index{}", next_tmp);
    //             *next_tmp += 1;
    //             name
    //         };
    //         let mut permutation = args.into_iter().enumerate().collect::<Vec<_>>();
    //         permutation.sort_by_key(|(_, arg)| ordering.iter().position(|o| o == arg));
    //         let (permutation, args) = permutation.into_iter().unzip();
    //         indexes.push((name.clone(), f, permutation));
    //         Expression::apply(&name, args)
    //     } else {
    //         self.map1(|e| Ok(e.reorder(lets, indexes, next_tmp)))
    //             .unwrap()
    //     }
    // }

    // pub fn bound(&self, name: &Name, type_cache: &Cache<ValueType>) -> Option<Expression> {
    //     use Expression::*;
    //     match self {
    //         Intersect(e1, e2) => {
    //             match (e1.bound(name, type_cache), e2.bound(name, type_cache)) {
    //                 (Some(b1), Some(b2)) => Some(Intersect(box b1, box b2)),
    //                 (Some(b), None) | (None, Some(b)) => Some(b),
    //                 (None, None) => None,
    //             }
    //         }
    //         Apply(box f, box arg) => {
    //             if let Name(arg) = arg {
    //                 if arg == name {
    //                     // TODO want to check
    //                     // && !type_cache.get(f).is_function() {
    //                     // but we don't have types for indexes because we permuted them
    //                     return Some(
    //                         Apply(
    //                             box Apply(
    //                                 box Name("permute".to_owned()),
    //                                 box Seal(box Scalar(crate::Scalar::Number(1))),
    //                             ),
    //                             box Seal(box f.clone()),
    //                         )
    //                         .with_natives(&Native::stdlib()),
    //                     );
    //                 }
    //             }
    //             f.bound(name, type_cache)
    //         }
    //         Equal(box e1, box e2) => {
    //             if let Name(e1) = e1 {
    //                 if e1 == name {
    //                     return Some(e2.clone());
    //                 }
    //             }
    //             if let Name(e2) = e2 {
    //                 if e2 == name {
    //                     return Some(e1.clone());
    //                 }
    //             }
    //             None
    //         }
    //         Exists(_, box e) => e.bound(name, type_cache),
    //         _ => None,
    //     }
    // }

    // pub fn lower2(
    //     &self,
    //     scalar_cache: &Cache<bool>,
    //     type_cache: &Cache<ValueType>,
    // ) -> Result<Self, String> {
    //     use Expression::*;
    //     let mut next_tmp = 0;
    //     let arity = match type_cache.get(self).arity() {
    //         Arity::Exactly(a) => a,
    //         Arity::AtLeast(_) => 0,
    //     };
    //     let arg_names = (0..arity)
    //         .map(|_| {
    //             let name = format!("var{}", next_tmp);
    //             next_tmp += 1;
    //             name
    //         })
    //         .collect::<Vec<_>>();
    //     let args = arg_names
    //         .iter()
    //         .map(|name| Name(name.clone()))
    //         .collect::<Vec<_>>();
    //     let mut ordering = vec![];
    //     let predicate = self.contains(&args, scalar_cache, type_cache, &mut next_tmp, &mut lets)?;
    //     {
    //         let mut seen = HashSet::new();
    //         ordering.retain(|name| seen.insert(name.clone()));
    //     }
    //     let mut indexes = vec![];
    //     let predicate = predicate.reorder(
    //         &ordering
    //             .iter()
    //             .map(|name| Name(name.clone()))
    //             .collect::<Vec<_>>(),
    //         &mut indexes,
    //         &mut next_tmp,
    //     );
    //     let mut expr = If(
    //         box predicate.clone(),
    //         box args
    //             .into_iter()
    //             .fold(Something, |a, b| Product(box a, box b)),
    //         box Nothing,
    //     );
    //     for name in ordering.iter().rev() {
    //         expr = Apply(
    //             box predicate
    //                 .bound(name, type_cache)
    //                 .unwrap_or(Name("panic".to_owned())),
    //             box Abstract(name.clone(), box expr),
    //         );
    //     }
    //     for (name, source, permutation) in indexes.into_iter().rev() {
    //         expr = Let(
    //             name,
    //             box Apply(
    //                 box Apply(
    //                     box Abstract(
    //                         "tmp0".to_owned(),
    //                         box Abstract(
    //                             "tmp1".to_owned(),
    //                             box ApplyNative(
    //                                 Native {
    //                                     name: "permute".to_owned(),
    //                                     input_arity: 2,
    //                                     output_arity: 1,
    //                                     fun: Native::permute,
    //                                 },
    //                                 vec!["tmp0".to_owned(), "tmp1".to_owned()],
    //                             ),
    //                         ),
    //                     ),
    //                     box Seal(box permutation.into_iter().fold(Something, |expr, i| {
    //                         Product(box expr, box Scalar(self::Scalar::Number((i + 1) as i64)))
    //                     })),
    //                 ),
    //                 box Seal(box source),
    //             ),
    //             box expr,
    //         );
    //     }
    //     Ok(expr)
    // }
}
