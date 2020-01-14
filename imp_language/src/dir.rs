use crate::shared::*;

pub type Slot = usize;
pub type Column = usize;
pub type Key = Vec<Column>;
pub type JoinKey = Vec<(Column, Column)>;

pub enum Dir {
    Constant(Set),
    Union(Vec<Slot>),
    Intersect(Vec<Slot>),
    Product(Vec<Slot>),
    FilterEqual(Slot, Vec<(Column, Column)>),
    Project(Slot, Key),
    Negate(Slot, Slot),
    Equal(Slot, Slot, Slot),
    MapNative(Slot, Native, Key),
}

pub struct Dirs {
    pub dirs: Vec<Dir>,
}

enum Binding {
    Set(Slot),
    Fun(Expression),
    Scalar,
}

struct DirsContext<'a> {
    dirs: Vec<Dir>,
    env: Environment<(Vec<Name>, Binding)>,
    type_cache: &'a Cache<ValueType>,
}

impl DirsContext<'_> {
    fn arity(&self, slot: Slot) -> Option<usize> {
        use Dir::*;
        match self.dirs[slot] {
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
            FilterEqual(a, _) => self.arity(a),
            Project(_, key) => Some(key.len()),
            Negate(outer, _) | Equal(outer, _, _) => self.arity(outer),
            MapNative(a, native, _) => self.arity(a).map(|a_arity| a_arity + native.output_arity),
        }
    }

    fn dir(&mut self, dir: Dir) -> Slot {
        self.dirs.push(dir);
        self.dirs.len() - 1
    }
}

fn prefix_join_key(len: usize) -> JoinKey {
    (0..len).map(|i| (i, i)).collect()
}

// assumes names are unique
// should only be applied to something with finite type
impl Expression {
    fn into_dirs(
        &self,
        outer_slot: Slot,
        outer_names: &[Name],
        context: &mut DirsContext,
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
                context.dir(Dir::Equal(outer_slot, a_slot, b_slot))
            }
            Negate(a) => {
                let a_slot = a.into_dirs(outer_slot, outer_names, context)?;
                context.dir(Dir::Negate(outer_slot, a_slot))
            }
            Name(name) => {
                let (binding_names, binding) = context.env.lookup(name).unwrap();
                match binding {
                    Binding::Fun(fun) => {
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
                        // TODO bare scalar!?
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
                    context.env.bind(
                        name.clone(),
                        (outer_names.to_vec(), Binding::Fun(self.clone())),
                    );
                } else {
                    let value_slot = value.into_dirs(outer_slot, outer_names, context)?;
                    context.env.bind(
                        name.clone(),
                        (outer_names.to_vec(), Binding::Set(value_slot)),
                    );
                }
                let body_slot = body.into_dirs(outer_slot, outer_names, context)?;
                context.env.unbind();
                body_slot
            }
            If(cond, if_true, if_false) => return Err(format!("`if` should be gone by now")),
            Abstract(name, body) => {
                return Err(format!("Bare abstract!"));
            }
            Apply(a, b) => {
                // TODO
                // check for native

                // this is the hard part
                // need to figure out what everything is bound to
                // not sure how to represent it
            }
            Native(native) => {
                return Err(format!("Bare native!"));
            }
            Reduce(..) | Seal(..) | Unseal(..) | Solve(..) => return Err(format!("TODO")),
        })
    }
}
