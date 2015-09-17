use runtime::{self, hash, Kind};
use std::collections::HashSet;
use std::collections::HashMap;
use regex::Regex;
use std::path::Path;
use std::io::prelude::*;
use std::fs::File;
use std::rc::Rc;

pub type ViewId = String;
pub type VariableId = String;

#[derive(Clone, Debug)]
pub struct Program {
    pub ids: Vec<ViewId>,
    pub schedule: Vec<usize>,
    pub schemas: Vec<Vec<Kind>>,
    pub views: Vec<View>,
}

#[derive(Clone, Debug)]
pub enum View {
    Input(Input),
    Query(Query),
}

#[derive(Clone, Debug)]
pub struct Input {
    pub filename: String,
    pub columns: Vec<usize>,
}

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

fn ordered_union(xs: &Vec<VariableId>, ys: &Vec<VariableId>) -> Vec<VariableId> {
    let mut results = xs.clone();
    for y in ys {
        if xs.iter().find(|x| *x == y).is_none() {
            results.push(y.clone());
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
        for var in vars.iter() {
            match self.bindings.iter().position(|binding| match *binding { Some(ref bound) => bound == var, None => false }) {
                Some(ix) => {
                    let column: usize = self.kinds.iter().take(ix).map(|kind| kind.width()).sum();
                    for offset in 0..self.kinds[ix].width() {
                        key.push(column + offset);
                    }
                    kinds.push(self.kinds[ix]);
                    bindings.push(Some(var.clone()));
                },
                None => (), // this var isn't here to project
            }
        }
        (key, kinds, bindings)
    }

    fn join_key(&self, vars: &Vec<VariableId>) -> Vec<usize> {
        let mut key = vec![];
        for var in vars.iter() {
            match self.bindings.iter().position(|binding| match *binding { Some(ref bound) => bound == var, None => false }) {
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
                    joined_vars.extend(vars.intersection(&other_vars).cloned());
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

impl Input {
    pub fn compile(&self, schema: &[Kind], strings: &mut Vec<String>) -> runtime::Chunk {
        let mut tsv = String::new();
        File::open(&self.filename).unwrap().read_to_string(&mut tsv).unwrap();
        let mut lines = tsv.lines();
        lines.next(); // drop header
        let mut data = vec![];
        for line in lines {
            let fields = line.split("\t").collect::<Vec<_>>();
            for (kind, column) in schema.iter().zip(self.columns.iter()) {
                match *kind {
                    Kind::Id => data.push(fields[*column].parse::<u64>().unwrap()),
                    Kind::Number => data.push(fields[*column].parse::<f64>().unwrap() as u64),
                    Kind::Text => {
                        let field = fields[*column].to_owned();
                        data.push(hash(&field));
                        data.push(strings.len() as u64);
                        strings.push(field);
                    }
                }
            }
        }
        let row_width = schema.iter().map(|kind| kind.width()).sum();
        runtime::Chunk{data: data, row_width: row_width}
    }
}

impl Query {
    pub fn compile(&self, program: &Program) -> runtime::Query {
        let upstream = self.clauses.iter().map(|clause| {
            let ix = program.ids.iter().position(|id| *id == clause.view).unwrap();
            program.schedule[ix]
        }).collect();
        let mut state = State{
            actions: vec![],
            chunks: self.clauses.iter().map(|clause| {
                let ix = program.ids.iter().position(|id| *id == clause.view).unwrap();
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

fn parse_clause(text: &str) -> (ViewId, Vec<Option<VariableId>>, Vec<Option<Kind>>) {
    let var_re = Regex::new(r"_|\?[:alpha:]*(:[:alpha:]*)?").unwrap();
    let kind_re = Regex::new(r":[:alpha:]*").unwrap();
    let bindings = text.matches(&var_re).map(|var_text| {
        match var_text {
            "_" => None,
            _ => Some(kind_re.replace(var_text, ""))
        }
    }).collect();
    let kinds = text.matches(&var_re).map(|var_text| {
        var_text.matches(&kind_re).next().map(|kind_text| {
            match kind_text {
                    ":id" => Kind::Id,
                    ":text" => Kind::Text,
                    ":number" => Kind::Number,
                    other => panic!("Unknown kind: {:?}", other),
                }
            })
    }).collect();
    let view_id = var_re.replace_all(text, "_");
    (view_id, bindings, kinds)
}

impl Program {
    pub fn compile(&self) -> runtime::Program {
        let mut ids = vec![];
        let mut schemas = vec![];
        let mut states = vec![];
        let mut views = vec![];
        let mut downstreams = vec![];
        let mut dirty = vec![];
        let mut strings = vec![];

        let mut scheduled_ixes = self.schedule.iter().zip(0..self.ids.len()).collect::<Vec<_>>();
        scheduled_ixes.sort();
        for (_, program_ix) in scheduled_ixes {
            ids.push(hash(&self.ids[program_ix]));
            let schema = self.schemas[program_ix].clone();
            match self.views[program_ix] {
                View::Input(ref input) => {
                    let state = input.compile(&schema[..], &mut strings);
                    states.push(Rc::new(state));
                    views.push(runtime::View::Input);
                    dirty.push(false);
                }
                View::Query(ref query) => {
                    let runtime_query = query.compile(self);
                    let row_width = schema.iter().map(|kind| kind.width()).sum();
                    states.push(Rc::new(runtime::Chunk{data: vec![], row_width: row_width}));
                    views.push(runtime::View::Query(runtime_query));
                    dirty.push(true);
                }
            }
            schemas.push(schema);
            downstreams.push(vec![]);
        }

        for (schedule_ix, view) in views.iter().enumerate() {
            if let runtime::View::Query(ref query) = *view {
                for upstream_ix in query.upstream.iter() {
                    downstreams[*upstream_ix].push(schedule_ix);
                }
            }
        }

        runtime::Program{ids: ids, schemas: schemas, states: states, views: views, downstreams: downstreams, dirty: dirty, strings: strings}
    }
}

fn parse_view(text: &str) -> (ViewId, Vec<Kind>, View) {
    let lines = text.split("\n").collect::<Vec<_>>();
    let (view_id, bindings, kinds) = parse_clause(lines[0]);
    let select = bindings.into_iter().map(|binding| binding.unwrap()).collect();
    let schema = kinds.into_iter().map(|kind| kind.unwrap()).collect();
    let view = match lines[1].chars().next().unwrap() {
        '=' => {
            // TODO handle manual input
            let mut words = lines[1].split(" ");
            words.next().unwrap(); // drop "="
            let filename = words.next().unwrap().to_owned();
            let columns = words.map(|word| word.parse::<usize>().unwrap()).collect();
            View::Input(Input{filename: filename, columns: columns})
        }
        '+' => {
            let clauses = lines[2..].iter().map(|line| {
                let (view_id, bindings, kinds) = parse_clause(line);
                for kind in kinds.into_iter() {
                    assert_eq!(kind, None);
                }
                Clause{view: view_id, bindings: bindings}
            }).collect();
            View::Query(Query{clauses: clauses, select: select})
        }
        _ => panic!("What are this? {:?}", lines[1]),
    };
    (view_id, schema, view)
}

impl Program {
    pub fn parse(text: &str) -> Program {
        let mut ids = vec![];
        let mut schedule = vec![];
        let mut schemas = vec![];
        let mut views = vec![];
        for (ix, view_text) in text.split("\n\n").enumerate() {
            if view_text != "" {
                let (id, schema, view) = parse_view(view_text);
                ids.push(id);
                schedule.push(ix); // ie just scheduling in textual order for now
                schemas.push(schema);
                views.push(view);
            }
        }
        Program{ids: ids, schedule: schedule, schemas: schemas, views: views}
    }

    pub fn load<P: AsRef<Path>>(filenames: &[P]) -> Program {
        let mut text = String::new();
        for filename in filenames.iter() {
            File::open(filename).unwrap().read_to_string(&mut text);
            text.push_str("\n\n");
        }
        Program::parse(&text[..])
    }
}

// TODO
// check schemas match bindings
// check types
// check selects are bound

#[cfg(test)]
pub mod tests{
    use super::*;
    use runtime::{self, hash};
    use test::{Bencher, black_box};
    use std::rc::Rc;

    fn compile_metal() -> runtime::Query {
        use runtime::Kind::*;
        let ids = vec![
           "artist".to_owned(),
           "album".to_owned(),
           "track".to_owned(),
           "playlist_track".to_owned(),
           "playlist".to_owned(),
           "query".to_owned(),
        ];
        let schedule = vec![0,1,2,3,4,5];
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
                view: "artist".to_owned(),
                bindings: vec![Some("artist_id".to_owned()), Some("artist_name".to_owned())],
            },
            Clause{
                view: "album".to_owned(),
                bindings: vec![Some("album_id".to_owned()), None, Some("artist_id".to_owned())],
            },
            Clause{
                view: "track".to_owned(),
                bindings: vec![Some("track_id".to_owned()), None, Some("album_id".to_owned())],
            },
            Clause{
                view: "playlist_track".to_owned(),
                bindings: vec![Some("playlist_id".to_owned()), Some("track_id".to_owned())],
            },
            Clause{
                view: "playlist".to_owned(),
                bindings: vec![Some("playlist_id".to_owned()), Some("playlist_name".to_owned())],
            },
            Clause{
                view: "query".to_owned(),
                bindings: vec![Some("playlist_name".to_owned())],
            }
            ],
            select: vec!["artist_name".to_owned()],
        };
        let program = Program{ids: ids, schedule: schedule, schemas: schemas, views: vec![]};
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

    #[test]
    pub fn test_metal_parse() {
        let bootstrap_program = Program::load(&["data/chinook.imp", "data/metal.imp"]);
        let runtime_program = bootstrap_program.compile();
        println!("{:?}", runtime_program);
        assert!(false);
    }
}