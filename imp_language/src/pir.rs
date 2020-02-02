use crate::shared::*;

// Physical Intermediate Representation

pub type Slot = usize;
pub type Column = usize;

#[derive(Debug)]
pub enum Pir {
    Constant(Set),
    Union(Vec<Slot>),
    Join(Slot, Slot, Vec<(usize, usize)>),
    FilterScalar(Slot, Vec<(Column, Scalar)>),
    FilterEqual(Slot, Vec<(Column, Column)>),
    Project(Slot, Vec<Column>),
    Difference(Slot, Slot),
    ApplyNative(Slot, Native, Vec<usize>),
}

#[derive(Debug)]
pub struct Pirs {
    pub pirs: Vec<Pir>,
    pub slot: Slot,
}

impl Pirs {
    fn push(&mut self, pir: Pir) -> Slot {
        self.pirs.push(pir);
        self.pirs.len() - 1
    }

    fn project(&mut self, slot: Slot, from: &[Name], to: &[Name]) -> Slot {
        let permutation = to
            .iter()
            .map(|name| from.iter().position(|name2| name == name2).unwrap())
            .collect();
        self.push(Pir::Project(slot, permutation))
    }
}

impl Lirs {
    pub fn pirs(&self) -> Pirs {
        let mut pirs = Pirs {
            pirs: vec![],
            slot: 0,
        };
        // slot 0 is always None
        pirs.push(Pir::Constant(Set::from_iter(vec![])));
        // slot 1 is always Some
        pirs.push(Pir::Constant(Set::from_iter(vec![vec![]])));
        let mut env = Environment::new();
        for lir in &self.lirs {
            let slot = lir.pir_into(&env, &mut pirs);
            env.bind(lir.name.clone(), (slot, lir.args.clone()));
            pirs.slot = slot; // actually only care about last one
        }
        pirs
    }
}

impl ValueLir {
    fn pir_into(&self, env: &Environment<(Slot, Vec<Name>)>, pirs: &mut Pirs) -> Slot {
        match self.body {
            BooleanLir::None => 0,
            BooleanLir::Some => {
                assert_eq!(
                    self.args.len(),
                    0,
                    "Body is some but args is {:?}",
                    self.args
                );
                1
            }
            _ => {
                let (slot, vars) = self.body.pir_into(1, &[], env, pirs);
                pirs.project(slot, &vars, &self.args)
            }
        }
    }
}

impl BooleanLir {
    fn pir_into(
        &self,
        known_slot: Slot,
        known_vars: &[Name],
        env: &Environment<(Slot, Vec<Name>)>,
        pirs: &mut Pirs,
    ) -> (Slot, Vec<Name>) {
        use BooleanLir::*;
        // d!(self, known_slot, known_vars);
        match self {
            None | Some => panic!("{} nested in lir", self),
            Union(a, b) => {
                let (a_slot, a_vars) = a.pir_into(known_slot, known_vars, env, pirs);
                let (b_slot, b_vars) = b.pir_into(known_slot, known_vars, env, pirs);
                let vars = a_vars
                    .iter()
                    .filter(|name| b_vars.iter().position(|name2| *name == name2).is_some())
                    .cloned()
                    .collect::<Vec<_>>();
                assert!(known_vars
                    .iter()
                    .all(|name| vars.iter().position(|name2| name == name2).is_some()));
                let a_slot = pirs.project(a_slot, &a_vars, &vars);
                let b_slot = pirs.project(b_slot, &b_vars, &vars);
                let slot = pirs.push(Pir::Union(vec![a_slot, b_slot]));
                (slot, vars)
            }
            Intersect(a, b) => {
                let (a_slot, a_vars) = a.pir_into(known_slot, known_vars, env, pirs);
                b.pir_into(a_slot, &a_vars, env, pirs)
            }
            If(cond, if_true, if_false) => {
                // TODO want to only calc cond once, but surprisingly tricky
                Union(
                    box Intersect(cond.clone(), if_true.clone()),
                    box Intersect(box Negate(cond.clone()), if_false.clone()),
                )
                .pir_into(known_slot, known_vars, env, pirs)
            }
            ScalarEqual(a, b) => match (a, b) {
                (ScalarRef::Name(a), ScalarRef::Name(b)) => {
                    let a_col = known_vars.iter().position(|name| name == a);
                    let b_col = known_vars.iter().position(|name| name == b);
                    match (a_col, b_col) {
                        (Option::Some(a_col), Option::Some(b_col)) => {
                            let slot =
                                pirs.push(Pir::FilterEqual(known_slot, vec![(a_col, b_col)]));
                            (slot, known_vars.to_vec())
                        }
                        (Option::Some(a_col), Option::None) => {
                            let mut projection = (0..known_vars.len()).collect::<Vec<_>>();
                            projection.push(a_col);
                            let slot = pirs.push(Pir::Project(known_slot, projection));
                            let mut known_vars = known_vars.to_vec();
                            known_vars.push(b.clone());
                            (slot, known_vars)
                        }
                        (Option::None, Option::Some(b_col)) => {
                            let mut projection = (0..known_vars.len()).collect::<Vec<_>>();
                            projection.push(b_col);
                            let slot = pirs.push(Pir::Project(known_slot, projection));
                            let mut known_vars = known_vars.to_vec();
                            known_vars.push(a.clone());
                            (slot, known_vars)
                        }
                        (Option::None, Option::None) => {
                            panic!("Neither of {} = {} bound in {:?}", a, b, known_vars)
                        }
                    }
                }
                (ScalarRef::Name(a), ScalarRef::Scalar(b))
                | (ScalarRef::Scalar(b), ScalarRef::Name(a)) => {
                    match known_vars.iter().position(|name| name == a) {
                        Option::Some(a_col) => {
                            let slot =
                                pirs.push(Pir::FilterScalar(known_slot, vec![(a_col, b.clone())]));
                            (slot, known_vars.to_vec())
                        }
                        Option::None => {
                            let constant =
                                pirs.push(Pir::Constant(Set::from_iter(vec![vec![b.clone()]])));
                            let slot = pirs.push(Pir::Join(known_slot, constant, vec![]));
                            let mut known_vars = known_vars.to_vec();
                            known_vars.push(a.clone());
                            (slot, known_vars)
                        }
                    }
                }
                (ScalarRef::Scalar(a), ScalarRef::Scalar(b)) => {
                    if a == b {
                        Some.pir_into(known_slot, known_vars, env, pirs)
                    } else {
                        None.pir_into(known_slot, known_vars, env, pirs)
                    }
                }
            },
            Negate(a) => {
                let (a_slot, a_vars) = a.pir_into(known_slot, known_vars, env, pirs);
                let a_slot = pirs.project(a_slot, &a_vars, known_vars);
                let slot = pirs.push(Pir::Difference(known_slot, a_slot));
                (slot, known_vars.to_vec())
            }
            Apply(f, args) => match f {
                ValueRef::Name(f) => {
                    let (f_slot, f_vars) = env.lookup(f).unwrap();
                    let mut filter_equal = vec![];
                    let mut filter_scalar = vec![];
                    let mut join = vec![];
                    let mut selection = (0..known_vars.len()).collect::<Vec<_>>();
                    let mut selected_vars = known_vars.to_vec();
                    for (f_col, scalar_ref) in args.iter().enumerate() {
                        match scalar_ref {
                            ScalarRef::Name(name) => {
                                match f_vars[0..f_col].iter().position(|name2| name == name2) {
                                    Option::Some(prev_f_col) => {
                                        filter_equal.push((prev_f_col, f_col));
                                    }
                                    Option::None => {
                                        match known_vars.iter().position(|name2| name == name2) {
                                            Option::Some(known_col) => {
                                                join.push((known_col, f_col));
                                            }
                                            Option::None => {
                                                selection.push(known_vars.len() + f_col);
                                                selected_vars.push(name.clone());
                                            }
                                        }
                                    }
                                }
                            }
                            ScalarRef::Scalar(scalar) => {
                                filter_scalar.push((f_col, scalar.clone()));
                            }
                        }
                    }
                    let f_slot = pirs.push(Pir::FilterEqual(*f_slot, filter_equal));
                    let f_slot = pirs.push(Pir::FilterScalar(f_slot, filter_scalar));
                    let join = pirs.push(Pir::Join(known_slot, f_slot, join));
                    let slot = pirs.push(Pir::Project(join, selection));
                    (slot, selected_vars)
                }
                ValueRef::Native(native) => {
                    let mut constant = vec![];
                    let mut apply = vec![];
                    for scalar_ref in &args[..native.input_arity] {
                        match scalar_ref {
                            ScalarRef::Name(name) => {
                                let known_col =
                                    known_vars.iter().position(|name2| name == name2).unwrap();
                                apply.push(known_col);
                            }
                            ScalarRef::Scalar(scalar) => {
                                constant.push(scalar.clone());
                                apply.push(known_vars.len() + constant.len() - 1);
                            }
                        }
                    }
                    let mut filter_equal = vec![];
                    let mut filter_scalar = vec![];
                    let mut selection = (0..known_vars.len()).collect::<Vec<_>>();
                    let mut selected_vars = known_vars.to_vec();
                    for (new_col, scalar_ref) in args[native.input_arity..].iter().enumerate() {
                        match scalar_ref {
                            ScalarRef::Name(name) => {
                                match known_vars.iter().position(|name2| name == name2) {
                                    Option::Some(known_col) => {
                                        filter_equal.push((known_col, known_vars.len() + new_col));
                                    }
                                    Option::None => {
                                        selection.push(known_vars.len() + new_col);
                                        selected_vars.push(name.clone());
                                    }
                                }
                            }
                            ScalarRef::Scalar(scalar) => {
                                filter_scalar.push((known_vars.len() + new_col, scalar.clone()));
                            }
                        }
                    }
                    let constant = pirs.push(Pir::Constant(Set::from_iter(vec![constant])));
                    let product = pirs.push(Pir::Join(known_slot, constant, vec![]));
                    let apply = pirs.push(Pir::ApplyNative(product, native.clone(), apply));
                    let apply = pirs.push(Pir::FilterEqual(apply, filter_equal));
                    let apply = pirs.push(Pir::FilterScalar(apply, filter_scalar));
                    let slot = pirs.push(Pir::Project(apply, selection));
                    (slot, selected_vars)
                }
            },
        }
    }
}

impl Pirs {
    pub fn eval(&self) -> Result<Set, String> {
        let mut data = self.pirs.iter().map(|_| Set::new()).collect::<Vec<_>>();
        for (slot, pir) in self.pirs.iter().enumerate() {
            use Pir::*;
            d!(slot, &pir);
            data[slot] = match pir {
                Constant(set) => set.clone(),
                Union(slots) => {
                    let mut set = data[slots[0]].clone();
                    for slot in &slots[1..] {
                        set = set.union(&data[*slot]).cloned().collect();
                    }
                    set
                }
                Join(a, b, key) => {
                    let mut index = HashMap::new();
                    for a_row in data[*a].iter() {
                        let k = key
                            .iter()
                            .map(|(a_col, _)| &a_row[*a_col])
                            .collect::<Vec<_>>();
                        index.entry(k).or_insert_with(|| vec![]).push(a_row)
                    }
                    let mut result = Set::new();
                    for b_row in data[*b].iter() {
                        let k = key
                            .iter()
                            .map(|(_, b_col)| &b_row[*b_col])
                            .collect::<Vec<_>>();
                        if let Option::Some(a_rows) = index.get(&k) {
                            for a_row in a_rows {
                                result.insert(a_row.iter().chain(b_row.iter()).cloned().collect());
                            }
                        }
                    }
                    result
                }
                FilterScalar(slot, scalars) => data[*slot]
                    .iter()
                    .filter(|row| scalars.iter().all(|(a, s)| row[*a] == *s))
                    .cloned()
                    .collect(),
                FilterEqual(slot, eqs) => data[*slot]
                    .iter()
                    .filter(|row| eqs.iter().all(|(a, b)| row[*a] == row[*b]))
                    .cloned()
                    .collect(),
                Project(slot, cols) => data[*slot]
                    .iter()
                    .map(|row| cols.iter().map(|col| row[*col].clone()).collect())
                    .collect(),
                Difference(a, b) => data[*a].difference(&data[*b]).cloned().collect(),
                ApplyNative(slot, native, key) => {
                    let mut set = Set::new();
                    for row in &data[*slot] {
                        let inputs = key.iter().map(|i| row[*i].clone()).collect();
                        let outputs = (native.fun)(inputs)?;
                        for output in outputs {
                            set.insert(row.iter().cloned().chain(output).collect());
                        }
                    }
                    set
                }
            };
            d!(&data[slot]);
            debug!("");
        }
        Ok(data[self.slot].clone())
    }
}
