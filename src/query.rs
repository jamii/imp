use relation::*;
use plan::{hash, Action, Plan};

use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Query {
    pub clauses: Vec<Clause>,
    pub select: Vec<VariableId>,
}

#[derive(Clone, Debug)]
pub struct Clause {
    pub view: ViewId,
    pub bindings: Vec<Option<VariableId>>,
}

#[derive(Clone, Debug)]
pub struct State {
    pub actions: Vec<Action>,
    pub chunks: Vec<Chunk>,
    pub to_join: Vec<usize>,
    pub to_select: Vec<VariableId>,
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub kinds: Vec<Kind>,
    pub bindings: Vec<Option<VariableId>>,
}

fn ordered_union(xs: &Vec<Id>, ys: &Vec<Id>) -> Vec<Id> {
    let mut results = xs.clone();
    for &y in ys {
        if xs.iter().find(|&&x| x == y).is_none() {
            results.push(y);
        }
    }
    results
}

impl Chunk {
    fn vars(&self) -> HashSet<VariableId> {
        self.bindings.iter().flat_map(|binding| binding.iter()).cloned().collect()
    }

    fn project_key(&self, vars: &Vec<VariableId>) -> (Vec<usize>, Vec<Kind>, Vec<Option<VariableId>>) {
        let mut key = vec![];
        let mut kinds = vec![];
        let mut bindings = vec![];
        for &var in vars.iter() {
            match self.bindings.iter().position(|&binding| binding == Some(var)) {
                Some(ix) => {
                    let column: usize = self.kinds.iter().take(ix).map(|kind| kind.width()).sum();
                    for offset in 0..self.kinds[ix].width() {
                        key.push(column + offset);
                    }
                    kinds.push(self.kinds[ix]);
                    bindings.push(Some(var));
                },
                None => (), // this var isn't here to project
            }
        }
        (key, kinds, bindings)
    }

    fn join_key(&self, vars: &Vec<VariableId>) -> Vec<usize> {
        let mut key = vec![];
        for &var in vars.iter() {
            match self.bindings.iter().position(|&binding| binding == Some(var)) {
                Some(ix) => {
                    let column = self.kinds.iter().take(ix).map(|kind| kind.width()).sum();
                    key.push(column);
                },
                None => (), // this var isn't here to project
            }
        }
        key
    }
}

impl State {
    pub fn find_ear(&self) -> (usize, usize) {
        for &chunk_ix in self.to_join.iter() {
            let chunk = &self.chunks[chunk_ix];
            let vars = chunk.vars();
            let mut joined_vars = HashSet::new();
            for &other_chunk_ix in self.to_join.iter() {
                if chunk_ix != other_chunk_ix {
                    let other_vars = self.chunks[other_chunk_ix].vars();
                    joined_vars.extend(vars.intersection(&other_vars));
                }
            }
            for &other_chunk_ix in self.to_join.iter() {
                if chunk_ix != other_chunk_ix {
                    let other_vars = self.chunks[other_chunk_ix].vars();
                    if joined_vars.is_subset(&other_vars) {
                        return (chunk_ix, other_chunk_ix);
                    }
                }
            }
        }
        panic!("Cant find an ear in:\n {:#?}", self);
    }

    pub fn project(&mut self, chunk_ix: usize, vars: &Vec<VariableId>) {
        let (key, kinds, bindings) = self.chunks[chunk_ix].project_key(vars);
        self.actions.push(Action::Project(chunk_ix, key));
        self.chunks[chunk_ix] = Chunk{kinds: kinds, bindings: bindings};
    }

    pub fn semijoin(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>) {
        let left_key = self.chunks[left_chunk_ix].join_key(vars);
        let right_key = self.chunks[right_chunk_ix].join_key(vars);
        assert_eq!(left_key.len(), right_key.len());
        self.actions.push(Action::SemiJoin(left_chunk_ix, right_chunk_ix, left_key, right_key));
    }

    pub fn join(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>) {
        let left_key = self.chunks[left_chunk_ix].join_key(vars);
        let right_key = self.chunks[right_chunk_ix].join_key(vars);
        assert_eq!(left_key.len(), right_key.len());
        self.actions.push(Action::Join(left_chunk_ix, right_chunk_ix, left_key, right_key));
        let mut left_kinds = ::std::mem::replace(&mut self.chunks[left_chunk_ix].kinds, vec![]);
        let right_kinds = ::std::mem::replace(&mut self.chunks[right_chunk_ix].kinds, vec![]);
        left_kinds.extend(right_kinds);
        let mut left_bindings = ::std::mem::replace(&mut self.chunks[left_chunk_ix].bindings, vec![]);
        let right_bindings = ::std::mem::replace(&mut self.chunks[right_chunk_ix].bindings, vec![]);
        left_bindings.extend(right_bindings);
        self.chunks[right_chunk_ix].kinds = left_kinds;
        self.chunks[right_chunk_ix].bindings = left_bindings;
    }

    pub fn plan(&mut self) -> usize {
        let to_select = self.to_select.clone();
        if self.to_join.len() == 2 {
            let left_ix = self.to_join[0];
            let right_ix = self.to_join[1];
            let left_vars = self.chunks[left_ix].vars();
            let right_vars = self.chunks[right_ix].vars();
            let join_vars = left_vars.intersection(&right_vars).cloned().collect();
            self.project(left_ix, &ordered_union(&join_vars, &to_select));
            self.project(right_ix, &ordered_union(&join_vars, &to_select));
            self.join(left_ix, right_ix, &join_vars);
            self.project(right_ix, &to_select);
            right_ix
        } else {
            let (ear_ix, parent_ix) = self.find_ear();
            let ear_vars = self.chunks[ear_ix].vars();
            let parent_vars = self.chunks[parent_ix].vars();
            let join_vars = ear_vars.intersection(&parent_vars).cloned().collect();
            self.project(ear_ix, &ordered_union(&join_vars, &to_select));
            self.project(parent_ix, &ordered_union(&join_vars, &parent_vars.iter().cloned().collect()));
            self.semijoin(ear_ix, parent_ix, &join_vars);
            self.to_join.retain(|&ix| ix != ear_ix);
            self.to_select = ordered_union(&join_vars, &to_select);
            let result_ix = self.plan();
            self.join(ear_ix, result_ix, &join_vars);
            self.project(result_ix, &to_select);
            result_ix
        }
    }
}

impl Query {
    pub fn plan(&self, schema: HashMap<ViewId, Vec<Kind>>) -> Plan {
        let mut state = State{
            actions: vec![],
            chunks: self.clauses.iter().map(|clause| {
                Chunk{
                    kinds: schema[&clause.view].clone(),
                    bindings: clause.bindings.clone(),
                }
            }).collect(),
            to_join: (0..self.clauses.len()).collect(),
            to_select: self.select.clone(),
        };
        let result = state.plan();
        Plan{actions: state.actions, result: result}
    }
}

#[cfg(test)]
mod tests{
    use super::{Clause, Query};
    use chunk::Chunk;
    use plan::{hash, Plan};
    use test::{Bencher, black_box};

    fn plan_metal() -> Plan {
        use relation::Kind::*;

        // variable ids
        let artist_name = 0;
        let artist_id = 1;
        let album_id = 2;
        let track_id = 3;
        let playlist_id = 4;
        let playlist_name = 5;

        let schema = vec![
            (0, vec![Id, Text]),
            (1, vec![Id, Text, Id]),
            (2, vec![Id, Text, Id]),
            (3, vec![Id, Id]),
            (4, vec![Id, Text]),
            (5, vec![Text]),
        ].into_iter().collect();
        let query = Query{
            clauses: vec![
            Clause{
                view: 0,
                bindings: vec![Some(artist_id), Some(artist_name)],
            },
            Clause{
                view: 1,
                bindings: vec![Some(album_id), None, Some(artist_id)],
            },
            Clause{
                view: 2,
                bindings: vec![Some(track_id), None, Some(album_id)],
            },
            Clause{
                view: 3,
                bindings: vec![Some(playlist_id), Some(track_id)],
            },
            Clause{
                view: 4,
                bindings: vec![Some(playlist_id), Some(playlist_name)],
            },
            Clause{
                view: 5,
                bindings: vec![Some(playlist_name)]
            }
            ],
            select: vec![artist_name],
        };
        query.plan(schema)
    }

    #[test]
    fn test_metal_plan() {
        let plan = plan_metal();
        let (mut strings, mut chunks) = ::plan::tests::chinook();
        let metal = "Heavy Metal Classic".to_owned();
        let query = Chunk{ data: vec![hash(&metal), strings.len() as u64], row_width: 2};
        strings.push(metal);
        chunks.push(query);
        let results = plan.execute(&strings, chunks);
        assert_set_eq!(
            results.data.chunks(2).map(|chunk| &strings[chunk[1] as usize][..]),
            vec!["AC/DC", "Accept", "Black Sabbath", "Metallica", "Iron Maiden", "Mot\u{f6}rhead", "M\u{f6}tley Cr\u{fc}e", "Ozzy Osbourne", "Scorpions"]
            );
    }

    #[bench]
    pub fn bench_metal_plan(bencher: &mut Bencher) {
        let plan = plan_metal();
        let (mut strings, mut chunks) = ::plan::tests::chinook();
        let metal = "Heavy Metal Classic".to_owned();
        let query = Chunk{ data: vec![hash(&metal), strings.len() as u64], row_width: 2};
        strings.push(metal);
        chunks.push(query);
        bencher.iter(|| {
            black_box(plan.execute(&strings, chunks.clone()));
        });
    }
}