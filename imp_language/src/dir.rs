use crate::shared::*;

pub type Slot = usize;
pub type Column = usize;

#[derive(Debug)]
pub enum Dir {
    Constant(Set),
    Union(Vec<Slot>),
    Intersect(Vec<Slot>),
    Product(Vec<Slot>),
    FilterEqual(Slot, Vec<(Column, Column)>),
    Project(Slot, Vec<Column>),
    Difference(Slot, Slot),
    ApplyNative(Slot, Native),
}

#[derive(Debug)]
pub struct Dirs {
    pub dirs: Vec<Dir>,
}

#[derive(Debug)]
enum Binding {
    Set(Slot),
    Fun(Expression),
    Scalar,
}

#[derive(Debug)]
pub struct DirsContext<'a> {
    pub dirs: RefCell<Vec<Dir>>,
    env: RefCell<Environment<(Vec<Name>, Binding)>>,
    type_cache: &'a Cache<ValueType>,
    gensym: &'a Gensym,
}

impl<'a> DirsContext<'a> {
    pub fn new(type_cache: &'a Cache<ValueType>, gensym: &'a Gensym) -> Self {
        DirsContext {
            dirs: RefCell::new(vec![Dir::Constant(Set::from_iter(vec![vec![]]))]),
            env: RefCell::new(Environment::new()),
            type_cache: &type_cache,
            gensym: &gensym,
        }
    }

    fn arity(&self, slot: Slot) -> Option<usize> {
        use Dir::*;
        match &self.dirs.borrow()[slot] {
            Constant(set) => match set.iter().next() {
                None => None,
                Some(tuple) => Some(tuple.len()),
            },
            Union(slots) | Intersect(slots) => {
                slots.iter().next().and_then(|slot| self.arity(*slot))
            }
            Product(slots) => slots
                .iter()
                .map(|slot| self.arity(*slot))
                .fold(None, |a, b| a.and_then(|a| b.map(|b| a + b))),
            FilterEqual(a, _) => self.arity(*a),
            Project(_, key) => Some(key.len()),
            Difference(a, _) => self.arity(*a),
            ApplyNative(a, native) => self
                .arity(*a)
                .map(|a_arity| a_arity - native.input_arity + native.output_arity),
        }
    }

    pub fn dir(&self, dir: Dir) -> Slot {
        let mut dirs = self.dirs.borrow_mut();
        dirs.push(dir);
        dirs.len() - 1
    }
}

// TODO
// assumes names are unique
// should only be applied to something with finite type
impl Expression {
    pub fn into_dirs(
        &self,
        outer_slot: Slot,
        outer_names: &[Name],
        context: &DirsContext,
    ) -> Result<Slot, String> {
        use Expression::*;
        Ok(match self {
            None => context.dir(Dir::Constant(Set::from_iter(vec![]))),
            Some => outer_slot,
            Scalar(scalar) => context.dir(Dir::Product(vec![
                outer_slot,
                context.dir(Dir::Constant(Set::from_iter(vec![vec![scalar.clone()]]))),
            ])),
            Union(a, b) => {
                let a_slot = a.into_dirs(outer_slot, outer_names, context)?;
                let b_slot = b.into_dirs(outer_slot, outer_names, context)?;
                context.dir(Dir::Union(vec![a_slot, b_slot]))
            }
            Intersect(a, b) => {
                let a_slot = a.into_dirs(outer_slot, outer_names, context)?;
                let b_slot = b.into_dirs(outer_slot, outer_names, context)?;
                context.dir(Dir::Intersect(vec![a_slot, b_slot]))
            }
            Product(a, b) => {
                let a_slot = a.into_dirs(outer_slot, outer_names, context)?;
                let b_slot = b.into_dirs(outer_slot, outer_names, context)?;
                let outer_arity = context.arity(outer_slot).unwrap_or(0);
                let a_arity = context.arity(a_slot).unwrap_or(0);
                let b_arity = context.arity(b_slot).unwrap_or(0);
                context.dir(Dir::Project(
                    context.dir(Dir::FilterEqual(
                        context.dir(Dir::Product(vec![a_slot, b_slot])),
                        (0..outer_arity).map(|i| (i, a_arity + i)).collect(),
                    )),
                    (0..a_arity)
                        .chain(a_arity + outer_arity..a_arity + b_arity)
                        .collect(),
                ))
            }
            Equal(a, b) => {
                let a_slot = a.into_dirs(outer_slot, outer_names, context)?;
                let b_slot = b.into_dirs(outer_slot, outer_names, context)?;
                let outer_arity = context.arity(outer_slot).unwrap_or(0);
                context.dir(Dir::Difference(
                    context.dir(Dir::Difference(
                        outer_slot,
                        context.dir(Dir::Project(
                            context.dir(Dir::Difference(a_slot, b_slot)),
                            (0..outer_arity).collect(),
                        )),
                    )),
                    context.dir(Dir::Project(
                        context.dir(Dir::Difference(b_slot, a_slot)),
                        (0..outer_arity).collect(),
                    )),
                ))
            }
            Negate(a) => {
                let a_slot = a.into_dirs(outer_slot, outer_names, context)?;
                let outer_arity = context.arity(outer_slot).unwrap_or(0);
                context.dir(Dir::Difference(
                    outer_slot,
                    context.dir(Dir::Project(a_slot, (0..outer_arity).collect())),
                ))
            }
            Name(name) => {
                let env = context.env.borrow();
                let (binding_names, binding) = env.lookup(name).unwrap();
                match binding {
                    Binding::Fun(_fun) => {
                        return Err(format!("Bare fun!"));
                    }
                    Binding::Set(binding_slot) => {
                        let mut join_key = vec![];
                        for (binding_ix, name) in binding_names.iter().enumerate() {
                            // must have come from a parent scope, so binding_names is contained in outer_names
                            let outer_ix =
                                outer_names.iter().position(|name2| name == name2).unwrap();
                            join_key.push((outer_ix, binding_ix));
                        }
                        let outer_arity = context.arity(outer_slot).unwrap_or(0);
                        let binding_outer_arity = binding_names.len();
                        let binding_arity = context.arity(*binding_slot).unwrap_or(0);
                        context.dir(Dir::Project(
                            context.dir(Dir::FilterEqual(
                                context.dir(Dir::Product(vec![outer_slot, *binding_slot])),
                                join_key,
                            )),
                            (0..outer_arity)
                                .chain(
                                    outer_arity + binding_outer_arity..outer_arity + binding_arity,
                                )
                                .collect(),
                        ))
                    }
                    Binding::Scalar => {
                        let outer_ix = outer_names.iter().position(|name2| name == name2).unwrap();
                        context.dir(Dir::Project(
                            outer_slot,
                            (0..outer_names.len()).chain(vec![outer_ix]).collect(),
                        ))
                    }
                }
            }
            Let(name, value, body) => {
                if context.type_cache.get(value).is_function() {
                    context.env.borrow_mut().bind(
                        name.clone(),
                        (outer_names.to_vec(), Binding::Fun(self.clone())),
                    );
                } else {
                    let value_slot = value.into_dirs(outer_slot, outer_names, context)?;
                    context.env.borrow_mut().bind(
                        name.clone(),
                        (outer_names.to_vec(), Binding::Set(value_slot)),
                    );
                }
                let body_slot = body.into_dirs(outer_slot, outer_names, context)?;
                context.env.borrow_mut().unbind();
                body_slot
            }
            If(_cond, _if_true, _if_false) => return Err(format!("`if` should be gone by now")),
            Abstract(_name, _body) => {
                return Err(format!("Bare abstract!"));
            }
            Apply(a, b) => {
                let mut a = a;
                let mut b = b;
                if context.type_cache.get(&a).is_function() {
                    std::mem::swap(&mut a, &mut b);
                }

                let a_slot = a.into_dirs(outer_slot, outer_names, context)?;

                fn apply_to(
                    outer_slot: Slot,
                    outer_names: &[crate::Name],
                    mut a_slot: Slot,
                    b: &Expression,
                    context: &DirsContext,
                ) -> Result<Slot, String> {
                    Ok(if !context.type_cache.get(&b).is_function() {
                        let mut b_slot = b.into_dirs(outer_slot, outer_names, context)?;
                        let mut a_arity = context.arity(a_slot).unwrap_or(0);
                        let mut b_arity = context.arity(b_slot).unwrap_or(0);
                        if a_arity > b_arity {
                            std::mem::swap(&mut a_slot, &mut b_slot);
                            std::mem::swap(&mut a_arity, &mut b_arity);
                        }
                        context.dir(Dir::Project(
                            context.dir(Dir::FilterEqual(
                                context.dir(Dir::Product(vec![a_slot, b_slot])),
                                (0..a_arity).map(|i| (i, a_arity + i)).collect(),
                            )),
                            (a_arity + a_arity..a_arity + b_arity).collect(),
                        ))
                    } else {
                        // if b is a fun, must be either name, abstract or native
                        match &b {
                            Name(name) => {
                                let env = context.env.borrow();
                                let (_binding_names, binding) = env.lookup(&name).unwrap();
                                // don't need binding_names because must be subset of outer_names
                                match binding {
                                    Binding::Fun(fun) => {
                                        apply_to(outer_slot, outer_names, a_slot, fun, context)?
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            Abstract(name, body) => {
                                let mut body = &**body;
                                let mut names = vec![name.clone()];
                                while let Abstract(name, body2) = body {
                                    names.push(name.clone());
                                    body = &**body2;
                                }

                                // funified!
                                assert!(!context.type_cache.get(&body).is_function());
                                if let Option::Some(a_arity) = context.arity(a_slot) {
                                    assert!(names.len() <= a_arity)
                                }

                                let a_arity = context.arity(a_slot).unwrap_or(0);
                                let extra_names = context.gensym.names(if a_arity > names.len() {
                                    a_arity - names.len()
                                } else {
                                    0
                                });
                                names.extend(extra_names.clone());

                                for name in &names {
                                    context.env.borrow_mut().bind(
                                        name.clone(),
                                        (outer_names.to_vec(), Binding::Scalar),
                                    );
                                }

                                let new_outer_slot = a_slot;
                                let new_outer_names = outer_names
                                    .iter()
                                    .chain(names.iter())
                                    .cloned()
                                    .collect::<Vec<_>>();
                                let ab_slot =
                                    body.into_dirs(new_outer_slot, &new_outer_names, context)?;
                                // TODO need to apply to product of extra names

                                for _ in &names {
                                    context.env.borrow_mut().unbind();
                                }

                                ab_slot
                            }
                            Native(native) => {
                                let outer_arity = context.arity(outer_slot).unwrap_or(0);
                                if let Option::Some(a_arity) = context.arity(a_slot) {
                                    // funified!
                                    assert_eq!(outer_arity + native.input_arity, a_arity);
                                }
                                context.dir(Dir::ApplyNative(a_slot, native.clone()))
                            }
                            _ => unreachable!(),
                        }
                    })
                }
                apply_to(outer_slot, outer_names, a_slot, &b, context)?
            }
            Native(_native) => {
                return Err(format!("Bare native!"));
            }
            Reduce(..) | Seal(..) | Unseal(..) | Solve(..) => return Err(format!("TODO")),
        })
    }
}
