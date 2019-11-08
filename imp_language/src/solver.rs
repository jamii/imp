use crate::shared::*;

// #[derive(Debug, Clone)]
// pub struct ContainsContext<'a> {
//     pub scalar_cache: &'a Cache<bool>,
//     pub type_cache: &'a Cache<ValueType>,
//     pub gensym: &'a Gensym,
// }

// impl Expression {
//     pub fn solve(&self, context: &ContainsContext) -> Result<Self, String> {
//         if let Expression::None = self {
//             Ok(Expression::None)
//         } else {
//             let arity = context.type_cache.get(self).arity();
//             let names = context.gensym.names(arity);
//             let args = names
//                 .iter()
//                 .map(|name| Expression::Name(name.clone()))
//                 .collect::<Vec<_>>();
//             self.contains(&args, context)?.solve_for(&names, context)
//         }
//     }

//     // self is a boolean expr over unbounded
//     // return Ok(bound) if possible - bound columns are in unbounded order
//     fn solve_for<'a>(
//         &self,
//         unbounded: &'a [Name],
//         context: &ContainsContext,
//     ) -> Result<Expression, String> {
//         use Expression::*;
//         Ok(match self {
//             None => None,
//             Some => {
//                 if unbounded.is_empty() {
//                     Some
//                 } else {
//                     Err(format!("{:?} unbounded in {}", unbounded, self))?
//                 }
//             }
//             Union(e1, e2) => Union(
//                 box e1.solve_for(unbounded, context)?,
//                 box e2.solve_for(unbounded, context)?,
//             ),
//             Intersect(..) => {
//                 // flatten out nested Intersect
//                 let mut stack = vec![self];
//                 let mut exprs = vec![];
//                 while let Option::Some(expr) = stack.pop() {
//                     match expr {
//                         Intersect(box e1, box e2) => {
//                             stack.push(e1);
//                             stack.push(e2);
//                         }
//                         _ => exprs.push(expr),
//                     }
//                 }

//                 // loop over exprs and greedily solve
//                 let mut solved = Some;
//                 let mut unbounded = unbounded.to_vec();
//                 let mut bounded_here = vec![];
//                 while !exprs.is_empty() {
//                     let mut to_remove = Option::None;
//                     for (i, expr) in exprs.iter().enumerate() {
//                         // try to bound some of unbounded
//                         // TODO eg none can bound more variables than are free - is that a problem?
//                         let free = expr.free_names();
//                         let mut to_bound = unbounded.clone();
//                         to_bound.retain(|name| free.contains(name));
//                         match expr.solve_for(&to_bound, context) {
//                             Ok(inner_solved) => {
//                                 to_remove = Option::Some(i);
//                                 solved = Apply(
//                                     box solved,
//                                     box Expression::_abstract(
//                                         bounded_here.clone(),
//                                         Expression::_product(inner_solved, bounded_here.clone()),
//                                     ),
//                                 );
//                                 unbounded.retain(|name| {
//                                     to_bound.iter().find(|name2| name == *name2).is_none()
//                                 });
//                                 bounded_here = to_bound
//                                     .into_iter()
//                                     .chain(bounded_here.into_iter())
//                                     .collect();
//                                 break;
//                             }
//                             Err(_) => (),
//                         }
//                     }
//                     if let Option::Some(to_remove) = to_remove {
//                         exprs.remove(to_remove);
//                     } else {
//                         Err(format!("{:?} unbounded in {}", unbounded, self))?
//                     }
//                 }

//                 solved
//             }
//             Apply(..) => {
//                 // flatten out nested Apply
//                 let mut fun = self;
//                 let mut args = vec![];
//                 while let Apply(box new_fun, box arg) = fun {
//                     fun = new_fun;
//                     args.push(arg);
//                 }
//                 args.reverse();

//                 // see if we can apply fun yet
//                 // TODO we need the type from before the contains transform - will the ref be preserved?
//                 let fun_arity = context.type_cache.get(fun).fun_arity();
//                 // fun is bounded if all it's free names are not in unbounded
//                 let fun_bounded = fun
//                     .free_names()
//                     .iter()
//                     .all(|name| unbounded.iter().find(|name2| name == *name2).is_none());
//                 // inputs are bounded if they are all scalars or names not in unbounded
//                 let inputs_bounded = args[..fun_arity].iter().all(|arg| match arg {
//                     Scalar(_) => true,
//                     Name(name) => unbounded.iter().find(|name2| name == *name2).is_none(),
//                     _ => unreachable!("contains should not emit this in arg position: {:?}", arg),
//                 });
//                 // outputs are complete if every unbounded appears at least once
//                 let outputs_complete = unbounded.iter().all(|name| {
//                     args[fun_arity..]
//                         .iter()
//                         .find(|arg| match arg {
//                             Scalar(_) => false,
//                             Name(name2) => name == name2,
//                             _ => unreachable!(
//                                 "contains should not emit this in arg position: {:?}",
//                                 arg
//                             ),
//                         })
//                         .is_some()
//                 });
//                 if !fun_bounded || !inputs_bounded || !outputs_complete {
//                     Err(format!("{:?} unbounded in {}", unbounded, self))?
//                 }

//                 // fun a b c
//                 // bounded = fun a c
//                 // unbounded = b
//                 // (fun a) (b c -> b)
//                 let output_names = context.gensym.names(args.len() - fun_arity);
//                 Apply(
//                     // apply fun to bounded inputs
//                     box Expression::apply(
//                         fun.clone(),
//                         args[..fun_arity].iter().map(|e| (*e).clone()).collect(),
//                     ),
//                     // map outputs to unbounded
//                     box Expression::_abstract(
//                         output_names.clone(),
//                         Expression::_product(
//                             Some,
//                             unbounded
//                                 .iter()
//                                 .map(|name| {
//                                     let pos = args[fun_arity..]
//                                         .iter()
//                                         .position(|arg| match arg {
//                                             Scalar(_) => false,
//                                             Name(name2) => name == name2,
//                                             _ => unreachable!("contains should not emit this in arg position: {:?}", arg),
//                                         })
//                                         .unwrap();
//                                     output_names[pos].clone()
//                                 })
//                                 .collect(),
//                         ),
//                     ),
//                 )
//             }
//             Seal(..) | Unseal(..) => Err(format!("Not handled in solve yet: {:?}", self))?,
//             Abstract(..) | Product(..) | Scalar(_) | Name(_) => {
//                 unreachable!("Contains should not emit this in bool position: {:?}", self)
//             }
//         })
//     }

//     // assumes self is in contains form
//     // returns None if self doesn't bound target
//     // fn bound_for(
//     //     &self,
//     //     target: &Name,
//     //     unbounded: &[Name],
//     //     negated: bool,
//     //     context: ContainsContext,
//     // ) -> Option<Expression> {
//     //     use Expression::*;
//     //     fn bound_union(b1: Option<Expression>, b2: Option<Expression>) -> Option<Expression> {
//     //         match (b1, b2) {
//     //             (Option::Some(b1), Option::Some(b2)) => Option::Some(Union(box b1, box b2)),
//     //             _ => Option::None,
//     //         }
//     //     }
//     //     fn bound_intersect(b1: Option<Expression>, b2: Option<Expression>) -> Option<Expression> {
//     //         match (b1, b2) {
//     //             (Option::Some(b1), Option::Some(b2)) => Option::Some(Intersect(box b1, box b2)),
//     //             (Option::Some(b), _) | (_, Option::Some(b)) => Option::Some(b),
//     //             _ => Option::None,
//     //         }
//     //     }
//     //     match self {
//     //         None => {
//     //             if !negated {
//     //                 Option::Some(None)
//     //             } else {
//     //                 Option::None
//     //             }
//     //         }
//     //         Some => {
//     //             if !negated {
//     //                 Option::None
//     //             } else {
//     //                 Option::Some(None)
//     //             }
//     //         }
//     //         Union(e1, e2) => {
//     //             let b1 = e1.bound_for(target, unbounded, negated, context);
//     //             let b2 = e2.bound_for(target, unbounded, negated, context);
//     //             if !negated {
//     //                 bound_union(b1, b2)
//     //             } else {
//     //                 bound_intersect(b1, b2)
//     //             }
//     //         }
//     //         Intersect(e1, e2) => {
//     //             let b1 = e1.bound_for(target, unbounded, negated, context);
//     //             let b2 = e2.bound_for(target, unbounded, negated, context);
//     //             if !negated {
//     //                 bound_intersect(b1, b2)
//     //             } else {
//     //                 bound_union(b1, b2)
//     //             }
//     //         }
//     //         Equal(..) => Option::None,
//     //         Negate(e) => e.bound_for(target, unbounded, !negated, context),
//     //         If(cond, if_true, if_false) => {
//     //             let bct = cond.bound_for(target, unbounded, true, context);
//     //             let bcf = cond.bound_for(target, unbounded, false, context);
//     //             let bt = if_true.bound_for(target, unbounded, negated, context);
//     //             let bf = if_false.bound_for(target, unbounded, negated, context);
//     //             if !negated {
//     //                 bound_union(bound_intersect(bct, bt), bound_intersect(bcf, bf))
//     //             } else {
//     //                 bound_intersect(bound_union(bct, bt), bound_union(bcf, bf))
//     //             }
//     //         }
//     //         Apply(..) => {
//     //             let mut fun = self;
//     //             let mut args = vec![];
//     //             while let Apply(box fun, box arg) = fun {
//     //                 args.push(arg);
//     //             }
//     //             args.reverse();
//     //             // TODO we need the type from before the contains transform - will the ref be preserved?
//     //             let fun_arity = context.type_cache.get(fun).fun_arity();
//     //             let fun_bounded = fun
//     //                 .free_names()
//     //                 .iter()
//     //                 .find(|name| *name == target)
//     //                 .is_none();
//     //             let args_bounded = args[..fun_arity].iter().all(|arg| match arg {
//     //                 Scalar(_) => true,
//     //                 Name(name) => unbounded.iter().find(|name2| name == *name2).is_none(),
//     //                 _ => unreachable!("contains should not emit this in arg position: {:?}", self),
//     //             });
//     //             if !fun_bounded || !args_bounded {
//     //                 return Option::None;
//     //             }
//     //             let target_pos = args[fun_arity..].iter().position(|arg| match arg {
//     //                 Name(name) => name == target,
//     //                 _ => false,
//     //             });
//     //             match target_pos {
//     //                 Option::None => Option::None,
//     //                 Option::Some(target_pos) => Option::Some(Expression::apply(
//     //                     fun.clone(),
//     //                     args[..target_pos]
//     //                         .iter()
//     //                         .map(|arg| (**arg).clone())
//     //                         .collect(),
//     //                 )),
//     //             }
//     //         }
//     //         Abstract(..) | Product(..) | Scalar(_) | Name(_) => {
//     //             unreachable!("contains should not emit this in bool position: {:?}", self)
//     //         }
//     //     }
//     // }
// }
