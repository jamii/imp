use runtime::{self, hash, Kind};
use std::collections::HashSet;
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
    pub bindings: Vec<Binding>,
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
    pub bindings: Vec<Binding>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Binding {
    Unbound,
    Constant(Value),
    Variable(VariableId),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Id(u64),
    Number(f64),
    Text(String),
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
        let mut vars = HashSet::new();
        for binding in self.bindings.iter() {
            match *binding {
                Binding::Variable(ref var) => vars.insert(var.clone()),
                _ => false,
            };
        }
        vars
    }

    fn project_key(&self, vars: &Vec<VariableId>) -> (Vec<usize>, Vec<Kind>, Vec<Binding>) {
        let mut key = vec![];
        let mut kinds = vec![];
        let mut bindings = vec![];
        for var in vars.iter() {
            match self.bindings.iter().position(|binding| match *binding { Binding::Variable(ref bound) => bound == var, _ => false }) {
                Some(ix) => {
                    let column: usize = self.kinds.iter().take(ix).map(|kind| kind.width()).sum();
                    for offset in 0..self.kinds[ix].width() {
                        key.push(column + offset);
                    }
                    kinds.push(self.kinds[ix]);
                    bindings.push(Binding::Variable(var.clone()));
                },
                None => (), // this var isn't here to project
            }
        }
        (key, kinds, bindings)
    }

    fn sort_key(&self, vars: &Vec<VariableId>) -> Vec<usize> {
        let mut key = vec![];
        for var in vars.iter() {
            match self.bindings.iter().position(|binding| match *binding { Binding::Variable(ref bound) => bound == var, _ => false }) {
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

    pub fn project(&mut self, chunk_ix: usize, sort_vars: &Vec<VariableId>, select_vars: &Vec<VariableId>) {
        let vars = ordered_union(sort_vars, select_vars);
        let num_fields = self.chunks[chunk_ix].bindings.len();
        let num_projected_vars = self.chunks[chunk_ix].vars().intersection(&vars.iter().cloned().collect()).count();
        if num_projected_vars == num_fields {
            // would project everything, might as well just sort
            let key = self.chunks[chunk_ix].sort_key(sort_vars);
            self.actions.push(runtime::Action::Sort(chunk_ix, key));
        } else {
            let (key, kinds, bindings) = self.chunks[chunk_ix].project_key(&vars);
            self.actions.push(runtime::Action::Project(chunk_ix, key));
            self.chunks[chunk_ix] = Chunk{kinds: kinds, bindings: bindings};
        }
    }

    pub fn semijoin(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>) {
        let left_key = self.chunks[left_chunk_ix].sort_key(vars);
        let right_key = self.chunks[right_chunk_ix].sort_key(vars);
        assert_eq!(left_key.len(), right_key.len());
        self.actions.push(runtime::Action::SemiJoin(left_chunk_ix, right_chunk_ix, left_key, right_key));
    }

    pub fn join(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>) {
        let left_key = self.chunks[left_chunk_ix].sort_key(vars);
        let right_key = self.chunks[right_chunk_ix].sort_key(vars);
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

    pub fn filter_and_selfjoin(&mut self, chunk_ix: usize) {
        let bindings = &self.chunks[chunk_ix].bindings;
        let kinds = &self.chunks[chunk_ix].kinds;
        for field in 0..bindings.len() {
            if let Binding::Constant(ref value) = bindings[field] {
                let column = kinds[0..field].iter().map(|kind| kind.width()).sum();
                let raw_value = match *value {
                    Value::Id(id) => id,
                    Value::Number(number) => number as u64,
                    Value::Text(ref text) => hash(text),
                };
                self.actions.push(runtime::Action::Filter(chunk_ix, column, raw_value));
            }
        }
        for left_field in 0..bindings.len() {
            for right_field in left_field+1..bindings.len() {
                if let Binding::Variable(_) = bindings[left_field] {
                    if bindings[left_field] == bindings[right_field] {
                        let left_column = kinds[0..left_field].iter().map(|kind| kind.width()).sum();
                        let right_column = kinds[0..right_field].iter().map(|kind| kind.width()).sum();
                        self.actions.push(runtime::Action::SelfJoin(chunk_ix, left_column, right_column));
                    }
                }
            }
        }
    }

    pub fn compile(&mut self) -> usize {
        let to_select = self.to_select.clone();
        match self.to_join.len() {
            1 => {
                let ix = self.to_join[0];
                self.project(ix, &vec![], &to_select);
                ix
            }
            2 => {
                let left_ix = self.to_join[0];
                let right_ix = self.to_join[1];
                let left_vars = self.chunks[left_ix].vars();
                let right_vars = self.chunks[right_ix].vars();
                let join_vars = left_vars.intersection(&right_vars).cloned().collect();
                self.project(left_ix, &join_vars, &to_select);
                self.project(right_ix, &join_vars, &to_select);
                self.join(left_ix, right_ix, &join_vars);
                self.project(right_ix, &vec![], &to_select);
                right_ix
            }
            _ => {
                let (ear_ix, parent_ix) = self.find_ear();
                let ear_vars = self.chunks[ear_ix].vars();
                let parent_vars = self.chunks[parent_ix].vars();
                let join_vars = ear_vars.intersection(&parent_vars).cloned().collect();
                self.project(ear_ix, &join_vars, &to_select);
                self.project(parent_ix, &join_vars, &parent_vars.iter().cloned().collect());
                self.semijoin(ear_ix, parent_ix, &join_vars);
                self.to_join.retain(|&ix| ix != ear_ix);
                self.to_select = ordered_union(&join_vars, &to_select);
                let result_ix = self.compile();
                self.join(ear_ix, result_ix, &join_vars);
                self.project(result_ix, &vec![], &to_select);
                result_ix
            }
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
        for chunk_ix in 0..state.chunks.len() {
            state.filter_and_selfjoin(chunk_ix);
        }
        let result = state.compile();
        runtime::Query{upstream: upstream, actions: state.actions, result: result}
    }
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

// We shall see that at which dogs howl in the dark, and that at which cats prick up their ears after midnight
fn parse_clause(text: &str) -> (ViewId, Vec<Binding>, Vec<Option<Kind>>) {
    let var_re = Regex::new(r#"_|\?[:alnum:]*(:[:alnum:]*)?|"[^"]*"|([:digit:]|\.)+"#).unwrap();
    let kind_re = Regex::new(r":[:alnum:]*").unwrap();
    let bindings = text.matches(&var_re).map(|var_text| {
        match var_text.chars().next().unwrap() {
            '_' => Binding::Unbound,
            '?' => Binding::Variable(kind_re.replace(var_text, "")),
            '#' => Binding::Constant(Value::Id(var_text[1..].parse::<u64>().unwrap())),
            '"' => Binding::Constant(Value::Text(var_text[1..var_text.len()-1].to_owned())),
            _ => Binding::Constant(Value::Number(var_text.parse::<f64>().unwrap())),
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

// If I am mad, it is mercy!
fn parse_view(text: &str) -> (ViewId, Vec<Kind>, View) {
    let lines = text.split("\n").collect::<Vec<_>>();
    let (view_id, bindings, kinds) = parse_clause(lines[0]);
    let select = bindings.into_iter().map(|binding| match binding { Binding::Variable(var) => var, _ => panic!() }).collect();
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
            File::open(filename).unwrap().read_to_string(&mut text).unwrap();
            text.push_str("\n\n");
        }
        Program::parse(&text[..])
    }
}

#[cfg(test)]
pub mod tests{
    use super::*;
    use test::{Bencher, black_box};

    #[test]
    pub fn test_metal() {
        let bootstrap_program = Program::load(&["data/chinook.imp", "data/metal.imp"]);
        let mut runtime_program = bootstrap_program.compile();
        runtime_program.run();
        assert_set_eq!(
            runtime_program.states[5].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["AC/DC", "Accept", "Black Sabbath", "Metallica", "Iron Maiden", "Mot\u{f6}rhead", "M\u{f6}tley Cr\u{fc}e", "Ozzy Osbourne", "Scorpions"]
            );
    }

    #[bench]
    pub fn bench_metal_run(bencher: &mut Bencher) {
        let bootstrap_program = Program::load(&["data/chinook.imp", "data/metal.imp"]);
        let runtime_program = bootstrap_program.compile();
        bencher.iter(|| {
            let mut runtime_program = runtime_program.clone();
            runtime_program.run();
            black_box(&runtime_program.states[5]);
        });
    }

    #[bench]
    pub fn bench_metal_all(bencher: &mut Bencher) {
        bencher.iter(|| {
            let bootstrap_program = Program::load(&["data/chinook.imp", "data/metal.imp"]);
            let mut runtime_program = bootstrap_program.compile();
            runtime_program.run();
            black_box(&runtime_program.states[5]);
        });
    }

    #[test]
    pub fn test_selfjoin() {
        let bootstrap_program = Program::load(&["data/chinook.imp", "data/selfjoin.imp"]);
        let mut runtime_program = bootstrap_program.compile();
        runtime_program.run();
        assert_set_eq!(
            runtime_program.states[5].data.iter().cloned(),
            vec![1,5,8]
            );
    }
}