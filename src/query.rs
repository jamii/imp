use relation::*;
use plan::{hash, Action, Plan};

use std::collections::HashSet;

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

    fn key(&self, vars: &Vec<VariableId>) -> Vec<usize> {
        let mut key = vec![];
        for &var in vars.iter() {
            let column = self.bindings.iter().position(|&binding| {
                binding == Some(var)
            });
            match column {
                Some(ix) => key.push(ix),
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
        let key = self.chunks[chunk_ix].key(vars);
        let bindings = key.iter().map(|&ix| self.chunks[chunk_ix].bindings[ix]).collect();
        self.actions.push(Action::Project(chunk_ix, key));
        self.chunks[chunk_ix] = Chunk{bindings: bindings};
    }

    pub fn semijoin(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>) {
        let left_key = self.chunks[left_chunk_ix].key(vars);
        let right_key = self.chunks[right_chunk_ix].key(vars);
        assert_eq!(left_key.len(), right_key.len());
        self.actions.push(Action::SemiJoin(left_chunk_ix, right_chunk_ix, left_key, right_key));
    }

    pub fn join(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>) {
        let left_key = self.chunks[left_chunk_ix].key(vars);
        let right_key = self.chunks[right_chunk_ix].key(vars);
        assert_eq!(left_key.len(), right_key.len());
        self.actions.push(Action::Join(left_chunk_ix, right_chunk_ix, left_key, right_key));
        let mut left_bindings = ::std::mem::replace(&mut self.chunks[left_chunk_ix].bindings, vec![]);
        let right_bindings = ::std::mem::replace(&mut self.chunks[right_chunk_ix].bindings, vec![]);
        left_bindings.extend(right_bindings);
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
    pub fn plan(&self) -> Plan {
        let mut state = State{
            actions: vec![],
            chunks: self.clauses.iter().map(|clause| {
                Chunk{bindings: clause.bindings.clone()}
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
    use plan::hash;
    use test::{Bencher, black_box};

    fn plan_metal(mut strings: Vec<String>, mut chunks: Vec<Chunk>) -> Chunk {
        let artist_name = 0;
        let artist_id = 1;
        let album_id = 2;
        let track_id = 3;
        let playlist_id = 4;
        let playlist_hash = 5;
        let query = Query{
            clauses: vec![
            Clause{
                view: 0,
                bindings: vec![Some(artist_id), None, Some(artist_name)],
            },
            Clause{
                view: 1,
                bindings: vec![Some(album_id), None, None, Some(artist_id)],
            },
            Clause{
                view: 2,
                bindings: vec![Some(track_id), None, None, Some(album_id)],
            },
            Clause{
                view: 3,
                bindings: vec![Some(playlist_id), Some(track_id)],
            },
            Clause{
                view: 4,
                bindings: vec![Some(playlist_id), Some(playlist_hash), None],
            },
            Clause{
                view: 5,
                bindings: vec![Some(playlist_hash), None]
            }
            ],
            select: vec![artist_name],
        };
        let plan = query.plan();
        let metal = "Heavy Metal Classic".to_owned();
        let query = Chunk{ data: vec![hash(&metal), strings.len() as u64], row_width: 2};
        strings.push(metal);
        chunks.push(query);
        plan.execute(&strings, chunks)
    }

    #[test]
    fn test_plan_metal() {
        let (strings, chunks) = ::plan::tests::chinook();
        let results = plan_metal(strings.clone(), chunks);
        assert_eq!(
            results.data.iter().map(|ix| &strings[*ix as usize]).collect::<Vec<_>>(),
            vec!["AC/DC", "Accept", "Black Sabbath", "Metallica", "Iron Maiden", "Mot\u{f6}rhead", "M\u{f6}tley Cr\u{fc}e", "Ozzy Osbourne", "Scorpions"]
            );
    }

    #[bench]
    pub fn bench_plan_metal(bencher: &mut Bencher) {
        let (strings, chunks) = ::plan::tests::chinook();
        bencher.iter(|| {
            black_box(plan_metal(strings.clone(), chunks.clone()));
        });
    }
}