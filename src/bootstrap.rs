use runtime::{self, hash, Id, ViewId, VariableId, Kind};
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
    pub actions: Vec<runtime::Action>,
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
        self.actions.push(runtime::Action::Project(chunk_ix, key));
        self.chunks[chunk_ix] = Chunk{kinds: kinds, bindings: bindings};
    }

    pub fn semijoin(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>) {
        let left_key = self.chunks[left_chunk_ix].join_key(vars);
        let right_key = self.chunks[right_chunk_ix].join_key(vars);
        assert_eq!(left_key.len(), right_key.len());
        self.actions.push(runtime::Action::SemiJoin(left_chunk_ix, right_chunk_ix, left_key, right_key));
    }

    pub fn join(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>) {
        let left_key = self.chunks[left_chunk_ix].join_key(vars);
        let right_key = self.chunks[right_chunk_ix].join_key(vars);
        assert_eq!(left_key.len(), right_key.len());
        self.actions.push(runtime::Action::Join(left_chunk_ix, right_chunk_ix, left_key, right_key));
        let mut left_kinds = ::std::mem::replace(&mut self.chunks[left_chunk_ix].kinds, vec![]);
        let right_kinds = ::std::mem::replace(&mut self.chunks[right_chunk_ix].kinds, vec![]);
        left_kinds.extend(right_kinds);
        let mut left_bindings = ::std::mem::replace(&mut self.chunks[left_chunk_ix].bindings, vec![]);
        let right_bindings = ::std::mem::replace(&mut self.chunks[right_chunk_ix].bindings, vec![]);
        left_bindings.extend(right_bindings);
        self.chunks[right_chunk_ix].kinds = left_kinds;
        self.chunks[right_chunk_ix].bindings = left_bindings;
    }

    pub fn compile(&mut self) -> usize {
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
            let result_ix = self.compile();
            self.join(ear_ix, result_ix, &join_vars);
            self.project(result_ix, &to_select);
            result_ix
        }
    }
}

impl Query {
    pub fn compile(&self, program: &Program) -> runtime::Query {
        let upstream = self.clauses.iter().map(|clause| {
            program.ids.iter().position(|&id| id == clause.view).unwrap()
        }).collect();
        let mut state = State{
            actions: vec![],
            chunks: self.clauses.iter().map(|clause| {
                let ix = program.ids.iter().position(|&id| id == clause.view).unwrap();
                Chunk{
                    kinds: program.schemas[ix].clone(),
                    bindings: clause.bindings.clone(),
                }
            }).collect(),
            to_join: (0..self.clauses.len()).collect(),
            to_select: self.select.clone(),
        };
        let result = state.compile();
        runtime::Query{upstream: upstream, actions: state.actions, result: result}
    }
}

pub struct Program{
    pub ids: Vec<ViewId>,
    pub schemas: Vec<Vec<Kind>>,
    pub views: Vec<View>,
}

pub enum View {
    Input,
    Query(Query),
}

// fn parse_clause(text: &str) -> (ViewId, Vec<Option<VariableId>>, Vec<Option<Kind>>) {
//     let var_re = Regex::new("\?[:alpha:]*")
//     let kind_re = Regex::new(":[:alpha:]*")
//     let bindings = text.matches(var_re).map(|var_text| {
//         match var_text {
//             "_" => None,
//             _ => Some(hash(var_text.replace(kind_re, "")))
//         }
//     }).collect();
//     let kinds = text.matches(var_re).map(|var_text| {
//         var_text.find(kind_re).map(|kind_text| {
//             match kind_text[1..] {
//                     "id" => Kind::Id,
//                     "text" => Kind::Text,
//                     "number" => Kind::Number,
//                     other => panic!("Unknown kind: {:?}", other),
//                 }
//             })
//     }).collect();
//     let view_id = hash(text.replace(var_re, "?"));
//     (view_id, bindings, kinds)
// }

// fn parse_view(text: &str) -> View {
//     let mut lines = text.split("\n").collect::<Vec<_>>;
//     let (view_id, bindings, kinds) = parse_clause(lines[0]);
//     let select = bindings.into_iter().map(|binding| binding.unwrap()).collect();
//     let schema = kinds.into_iter().map(|kind| kind.unwrap());
//     let node = match lines[1].char_at(0) {
//         "=" => {
//             // TODO handle inputs
//         }
//         "+" => {
//             // TODO handle query
//             let clauses = lines[2..].iter().map(|line| {
//                 let (view_id, bindings, kinds) = parse_clause(line);
//                 for kind in kinds.into_iter() {
//                     assert_eq!(kind, None);
//                 }
//                 Clause{view: view_id, bindings: bindings}
//             }).collect();
//             Node::Query(Query{
//                 clauses: clauses,
//                 select: select,
//             })
//         }
//     };
//     View{
//         id: view_id,
//         schema: schema,
//         node: node,
//     }
// }

// fn parse(text: &str) -> Program {
//     let mut errors = vec![];
//     let views = text.split("\n\n").map(|view_text| parse_view(view_text)).collect();
//     Program{views: view}
// }

// // TODO
// // check schemas match bindings
// // check types
// // check selects are bound

#[cfg(test)]
pub mod tests{
    use super::*;
    use runtime::{self, hash};
    use test::{Bencher, black_box};
    use std::rc::Rc;

    fn compile_metal() -> runtime::Query {
        use runtime::Kind::*;

        // variable ids
        let artist_name = 0;
        let artist_id = 1;
        let album_id = 2;
        let track_id = 3;
        let playlist_id = 4;
        let playlist_name = 5;


        let ids = vec![0,1,2,3,4,5];
        let schemas = vec![
            vec![Id, Text],
            vec![Id, Text, Id],
            vec![Id, Text, Id],
            vec![Id, Id],
            vec![Id, Text],
            vec![Text],
        ];
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
        let program = Program{ids: ids, schemas: schemas, views: vec![]};
        query.compile(&program)
    }

    #[test]
    fn test_metal_compile() {
        let query = compile_metal();
        let (mut strings, mut states) = runtime::tests::chinook();
        let metal = "Heavy Metal Classic".to_owned();
        let query_state = runtime::Chunk{ data: vec![hash(&metal), strings.len() as u64], row_width: 2};
        strings.push(metal);
        states.push(Rc::new(query_state));
        let results = query.execute(&strings, &states[..]);
        assert_set_eq!(
            results.data.chunks(2).map(|chunk| &strings[chunk[1] as usize][..]),
            vec!["AC/DC", "Accept", "Black Sabbath", "Metallica", "Iron Maiden", "Mot\u{f6}rhead", "M\u{f6}tley Cr\u{fc}e", "Ozzy Osbourne", "Scorpions"]
            );
    }

    #[bench]
    pub fn bench_metal_compile(bencher: &mut Bencher) {
        let query = compile_metal();
        let (mut strings, mut states) = runtime::tests::chinook();
        let metal = "Heavy Metal Classic".to_owned();
        let query_state = runtime::Chunk{ data: vec![hash(&metal), strings.len() as u64], row_width: 2};
        strings.push(metal);
        states.push(Rc::new(query_state));
        bencher.iter(|| {
            black_box(query.execute(&strings, &states[..]));
        });
    }
}