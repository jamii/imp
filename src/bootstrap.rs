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
    Union(Union),
}

#[derive(Clone, Debug)]
pub struct Input {
    pub tsv: Option<(String, Vec<usize>)>,
    pub rows: Vec<Vec<Value>>,
}

#[derive(Clone, Debug)]
pub struct Query {
    pub clauses: Vec<Clause>,
    pub select: Vec<VariableId>,
}

#[derive(Clone, Debug)]
pub struct Union {
    pub members: Vec<Member>,
}

#[derive(Clone, Debug)]
pub enum Member {
    Insert(ViewId),
    Remove(ViewId),
}

#[derive(Clone, Debug)]
pub struct Clause {
    pub view: ViewId,
    pub bindings: Vec<Binding>,
}

#[derive(Clone, Debug)]
pub struct QueryPlanner {
    pub actions: Vec<runtime::Action>,
    pub chunks: Vec<Chunk>,
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub kinds: Vec<Kind>,
    pub bindings: Vec<Binding>,
    pub bound_vars: HashSet<VariableId>,
}

#[derive(Clone, Debug)]
pub struct Primitive {
    pub primitive: runtime::Primitive,
    pub input_kinds: Vec<Kind>,
    pub output_kinds: Vec<Kind>,
    pub input_bindings: Vec<Binding>,
    pub output_bindings: Vec<Binding>,
    pub bound_input_vars: HashSet<VariableId>,
    pub bound_output_vars: HashSet<VariableId>,
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

// (child, parent) sorted from root downwards
pub type Tree = Vec<(usize, Option<usize>)>;

fn ordered_union(xs: &Vec<VariableId>, ys: &HashSet<VariableId>) -> Vec<VariableId> {
    let mut results = xs.clone();
    for y in ys {
        if xs.iter().find(|x| *x == y).is_none() {
            results.push(y.clone());
        }
    }
    results
}

fn bound_vars(bindings: &Vec<Binding>) -> HashSet<VariableId> {
    let mut vars = HashSet::new();
    for binding in bindings.iter() {
        match *binding {
            Binding::Variable(ref var) => vars.insert(var.clone()),
            _ => false,
        };
    }
    vars
}

fn find_join_ear(chunks: &Vec<Chunk>, unused: &Vec<usize>) -> (usize, usize) {
    for &child_ix in unused.iter() {
        let child_vars = &chunks[child_ix].bound_vars;
        let mut joined_vars = HashSet::new();
        for &other_ix in unused.iter() {
            if child_ix != other_ix {
                let other_vars = &chunks[other_ix].bound_vars;
                joined_vars.extend(child_vars.intersection(other_vars).cloned());
            }
        }
        for &parent_ix in unused.iter() {
            if child_ix != parent_ix {
                let parent_vars = &chunks[parent_ix].bound_vars;
                if joined_vars.is_subset(parent_vars) {
                    return (child_ix, parent_ix);
                }
            }
        }
    }
    panic!("Cant find an ear in: {:#?}", (chunks, unused));
}

fn build_join_tree(chunks: &Vec<Chunk>) -> Tree {
    assert!(chunks.len() > 0);
    let mut unused = (0..chunks.len()).collect::<Vec<_>>();
    let mut tree = vec![];
    while unused.len() > 1 { // one chunk will be left behind as the root
        let (child_ix, parent_ix) = find_join_ear(chunks, &unused);
        unused.retain(|ix| *ix != child_ix);
        tree.push((child_ix, Some(parent_ix)));
    }
    tree.push((unused[0], None));
    tree.reverse();
    tree
}

// the list is sorted by size and each subtree is sorted from leaves upwards
fn all_subtrees(tree: &Tree) -> Vec<Tree> {
    let mut subtrees = vec![];
    for &(child_ix, _) in tree.iter() {
        subtrees.push(vec![(child_ix, None)]);
    }
    for &(child_ix, parent_ix) in tree.iter() {
        let mut new_subtrees = vec![];
        for subtree in subtrees.iter() {
            if subtree.iter().any(|&(existing_child_ix, _)| Some(existing_child_ix) == parent_ix) {
                let mut new_subtree = subtree.clone();
                new_subtree.push((child_ix, parent_ix));
                new_subtrees.push(new_subtree);
            }
        }
        subtrees.extend(new_subtrees);
    }
    subtrees.sort_by(|a, b| b.len().cmp(&a.len()));
    subtrees
}

fn vars_in_tree(chunks: &Vec<Chunk>, tree: &Tree) -> HashSet<VariableId> {
    let mut vars = HashSet::new();
    for &(child_ix, _) in tree.iter() {
        vars.extend(chunks[child_ix].bound_vars.clone());
    }
    vars
}

fn cheapest_primitive_subtree(chunks: &Vec<Chunk>, join_tree: &Tree, primitives: &Vec<Primitive>) -> (usize, Tree) {
    for subtree in all_subtrees(join_tree).into_iter() {
        let vars = vars_in_tree(chunks, &subtree);
        for (primitive_ix, primitive) in primitives.iter().enumerate() {
            if primitive.bound_input_vars.is_subset(&vars) {
                return (primitive_ix, subtree);
            }
        }
    }
    panic!("Cannot schedule a primitive out of: {:#?}", (primitives, chunks, join_tree));
}

fn project_key(chunk: &Chunk, vars: &Vec<VariableId>) -> (Vec<usize>, Vec<Kind>, Vec<Binding>) {
    let mut key = vec![];
    let mut kinds = vec![];
    let mut bindings = vec![];
    for var in vars.iter() {
        match chunk.bindings.iter().position(|binding| match *binding { Binding::Variable(ref bound) => bound == var, _ => false }) {
            Some(ix) => {
                let column: usize = chunk.kinds.iter().take(ix).map(|kind| kind.width()).sum();
                for offset in 0..chunk.kinds[ix].width() {
                    key.push(column + offset);
                }
                kinds.push(chunk.kinds[ix]);
                bindings.push(Binding::Variable(var.clone()));
            },
            None => (), // can't project a var that isn't there
        }
    }
    (key, kinds, bindings)
}

fn sort_key(chunk: &Chunk, vars: &Vec<VariableId>) -> Vec<usize> {
    let mut key = vec![];
    for var in vars.iter() {
        match chunk.bindings.iter().position(|binding| match *binding { Binding::Variable(ref bound) => bound == var, _ => false }) {
            Some(ix) => {
                let column = chunk.kinds.iter().take(ix).map(|kind| kind.width()).sum();
                key.push(column);
            },
            None => (), // can't sort a var that isn't there
        }
    }
    key
}

pub fn filter(chunks: &mut Vec<Chunk>, actions: &mut Vec<runtime::Action>, chunk_ix: usize) {
    for field in 0..chunks[chunk_ix].bindings.len() {
        if let Binding::Constant(ref value) = chunks[chunk_ix].bindings[field] {
            let column = chunks[chunk_ix].kinds[0..field].iter().map(|kind| kind.width()).sum();
            let raw_value = match *value {
                Value::Id(id) => id,
                Value::Number(number) => number as u64,
                Value::Text(ref text) => hash(text),
            };
            actions.push(runtime::Action::Filter(chunk_ix, column, raw_value));
        }
        if let Binding::Constant(_) = chunks[chunk_ix].bindings[field] {
            chunks[chunk_ix].bindings[field] = Binding::Unbound;
        }
    }
}

fn selfjoin(chunks: &mut Vec<Chunk>, actions: &mut Vec<runtime::Action>, chunk_ix: usize) {
    let bindings = &chunks[chunk_ix].bindings;
    let kinds = &chunks[chunk_ix].kinds;
    for left_field in 0..bindings.len() {
        for right_field in left_field+1..bindings.len() {
            if let Binding::Variable(_) = bindings[left_field] {
                if bindings[left_field] == bindings[right_field] {
                    let left_column = kinds[0..left_field].iter().map(|kind| kind.width()).sum();
                    let right_column = kinds[0..right_field].iter().map(|kind| kind.width()).sum();
                    actions.push(runtime::Action::SelfJoin(chunk_ix, left_column, right_column));
                }
            }
        }
    }
}

pub fn sort_and_project(chunks: &mut Vec<Chunk>, actions: &mut Vec<runtime::Action>, chunk_ix: usize, sort_vars: &Vec<VariableId>, select_vars: &HashSet<VariableId>) {
    let vars = ordered_union(sort_vars, select_vars);
    let num_fields = chunks[chunk_ix].bindings.len();
    let num_projected_vars = bound_vars(&chunks[chunk_ix].bindings).intersection(&vars.iter().cloned().collect()).count();
    if num_projected_vars == num_fields {
        // would project everything, might as well just sort
        let key = sort_key(&chunks[chunk_ix], sort_vars);
        actions.push(runtime::Action::Sort(chunk_ix, key));
    } else {
        let (key, kinds, bindings) = project_key(&chunks[chunk_ix], &vars);
        actions.push(runtime::Action::Project(chunk_ix, key));
        let bound_vars = bound_vars(&bindings);
        chunks[chunk_ix] = Chunk{kinds: kinds, bindings: bindings, bound_vars: bound_vars};
    }
}

pub fn semijoin(chunks: &mut Vec<Chunk>, actions: &mut Vec<runtime::Action>, left_chunk_ix: usize, right_chunk_ix: usize) {
    let join_vars = chunks[left_chunk_ix].bound_vars.intersection(&chunks[right_chunk_ix].bound_vars).cloned().collect();
    let left_vars = chunks[left_chunk_ix].bound_vars.clone();
    let right_vars = chunks[right_chunk_ix].bound_vars.clone();
    sort_and_project(chunks, actions, left_chunk_ix, &join_vars, &left_vars);
    sort_and_project(chunks, actions, right_chunk_ix, &join_vars, &right_vars);
    let left_key = sort_key(&chunks[left_chunk_ix], &join_vars);
    let right_key = sort_key(&chunks[right_chunk_ix], &join_vars);
    assert_eq!(left_key.len(), right_key.len());
    actions.push(runtime::Action::SemiJoin(left_chunk_ix, right_chunk_ix, left_key, right_key));
}

pub fn join(chunks: &mut Vec<Chunk>, actions: &mut Vec<runtime::Action>, join_tree: &mut Tree, left_chunk_ix: usize, right_chunk_ix: usize, select: HashSet<VariableId>) {
    let join_vars = chunks[left_chunk_ix].bound_vars.intersection(&chunks[right_chunk_ix].bound_vars).cloned().collect();
    sort_and_project(chunks, actions, left_chunk_ix, &join_vars, &select);
    sort_and_project(chunks, actions, right_chunk_ix, &join_vars, &select);
    let left_key = sort_key(&chunks[left_chunk_ix], &join_vars);
    let right_key = sort_key(&chunks[right_chunk_ix], &join_vars);
    assert_eq!(left_key.len(), right_key.len());
    actions.push(runtime::Action::Join(left_chunk_ix, right_chunk_ix, left_key, right_key));
    let mut left_kinds = ::std::mem::replace(&mut chunks[left_chunk_ix].kinds, vec![]);
    let right_kinds = ::std::mem::replace(&mut chunks[right_chunk_ix].kinds, vec![]);
    left_kinds.extend(right_kinds);
    let mut left_bindings = ::std::mem::replace(&mut chunks[left_chunk_ix].bindings, vec![]);
    let right_bindings = ::std::mem::replace(&mut chunks[right_chunk_ix].bindings, vec![]);
    left_bindings.extend(right_bindings);
    let mut left_bound_vars = ::std::mem::replace(&mut chunks[left_chunk_ix].bound_vars, HashSet::with_capacity(0));
    let right_bound_vars = ::std::mem::replace(&mut chunks[right_chunk_ix].bound_vars, HashSet::with_capacity(0));
    left_bound_vars.extend(right_bound_vars);
    chunks[right_chunk_ix].kinds = left_kinds;
    chunks[right_chunk_ix].bindings = left_bindings;
    chunks[right_chunk_ix].bound_vars = left_bound_vars;
    join_tree.retain(|&(child_ix, _)| child_ix != left_chunk_ix);
    for edge in join_tree.iter_mut() {
        if edge.1 == Some(left_chunk_ix) {
            edge.1 = Some(right_chunk_ix);
        }
    }
    join_tree[0].1 = None;
}

pub fn collapse_subtree(chunks: &mut Vec<Chunk>, actions: &mut Vec<runtime::Action>, join_tree: &mut Tree, select: &Vec<VariableId>, primitives: &Vec<Primitive>, subtree: &Tree) -> usize {
    for edge in subtree.iter().rev() {
        if let &(child_ix, Some(parent_ix)) = edge {
            let mut vars = select.iter().cloned().collect::<HashSet<_>>();
            for (chunk_ix, chunk) in chunks.iter().enumerate() {
                if (chunk_ix != child_ix) && (chunk_ix != parent_ix) {
                    vars.extend(chunk.bound_vars.clone());
                }
            }
            for primitive in primitives.iter() {
                vars.extend(primitive.bound_input_vars.clone());
                vars.extend(primitive.bound_output_vars.clone());
            }
            join(chunks, actions, join_tree, child_ix, parent_ix, vars);
        }
    }
    subtree[0].0 // return the root ix
}

pub fn apply(chunks: &mut Vec<Chunk>, actions: &mut Vec<runtime::Action>, strings: &mut Vec<String>, chunk_ix: usize, primitive: &Primitive) {
    {
        let chunk = &mut chunks[chunk_ix];
        let num_columns: usize = chunk.kinds.iter().map(|kind| kind.width()).sum();
        let mut constants = vec![];
        let input_ixes = primitive.input_bindings.iter().map(|binding| {
            match *binding {
                Binding::Unbound => panic!("Unbound input in: {:#?}", primitive),
                Binding::Constant(ref constant) => {
                    let ix = constants.len();
                    match *constant {
                        Value::Id(id) => {
                            constants.push(id);
                            chunk.kinds.push(Kind::Id);
                            chunk.bindings.push(Binding::Unbound);
                        }
                        Value::Number(number) => {
                            constants.push(number as u64);
                            chunk.kinds.push(Kind::Number);
                            chunk.bindings.push(Binding::Unbound);
                        }
                        Value::Text(ref string) => {
                            constants.push(hash(string));
                            constants.push(strings.len() as u64);
                            strings.push(string.clone());
                            chunk.kinds.push(Kind::Text);
                            chunk.bindings.push(Binding::Unbound);
                        }
                    }
                    num_columns + ix
                },
                Binding::Variable(_) => {
                    let ix = chunk.bindings.iter().position(|chunk_binding| chunk_binding == binding).unwrap();
                    chunk.kinds.iter().take(ix).map(|kind| kind.width()).sum()
                }
            }
        }).collect();
        if constants.len() > 0 {
            // TODO this seems like an expensive solution to constant bindings in the inputs
            actions.push(runtime::Action::Extend(chunk_ix, constants));
        }
        actions.push(runtime::Action::Apply(chunk_ix, primitive.primitive, input_ixes));
        chunk.kinds.extend(primitive.output_kinds.clone());
        chunk.bindings.extend(primitive.output_bindings.clone());
        chunk.bound_vars.extend(primitive.bound_output_vars.clone());
    }
    filter(chunks, actions, chunk_ix); // handle any output vars that are constants
    selfjoin(chunks, actions, chunk_ix); // handle any output vars that need joining
}

fn as_primitive(view_id: &str, bindings: &Vec<Binding>) -> Option<Primitive> {
    use runtime::Primitive::*;
    use runtime::Kind::*;
    match (view_id, &bindings[..]) {
        ("_ = _ + _", [ref c, ref a, ref b]) => Some(Primitive{
            primitive: Add,
            input_kinds: vec![Number, Number],
            input_bindings: vec![a.clone(), b.clone()],
            output_kinds: vec![Number],
            output_bindings: vec![c.clone()],
            bound_input_vars: bound_vars(&vec![a.clone(), b.clone()]),
            bound_output_vars: bound_vars(&vec![c.clone()]),
        }),
        _ => None,
    }
}

pub fn compile_query(query: &Query, program: &Program, strings: &mut Vec<String>) -> runtime::Query {
    let mut upstream = vec![];
    let mut chunks = vec![];
    let mut primitives = vec![];
    for clause in query.clauses.iter() {
        let view = program.ids.iter().position(|id| *id == clause.view);
        let primitive = as_primitive(&clause.view, &clause.bindings);
        match (view, primitive) {
            (Some(ix), None) => {
                upstream.push(ix);
                chunks.push(Chunk{
                    kinds: program.schemas[ix].clone(),
                    bindings: clause.bindings.clone(),
                    bound_vars: bound_vars(&clause.bindings),
                });
            }
            (None, Some(primitive)) => {
                primitives.push(primitive)
            }
            other => panic!("What are this: {:#?}", (clause, other)),
        }
    }
    let mut join_tree = build_join_tree(&chunks);
    let mut actions = vec![];
    for chunk_ix in 0..chunks.len() {
        filter(&mut chunks, &mut actions, chunk_ix);
        selfjoin(&mut chunks, &mut actions, chunk_ix);
    }
    for edge in join_tree.iter().rev() {
        if let &(child_ix, Some(parent_ix)) = edge {
            semijoin(&mut chunks, &mut actions, child_ix, parent_ix);
        }
    }
    for edge in join_tree.iter().rev() {
        if let &(child_ix, Some(parent_ix)) = edge {
            semijoin(&mut chunks, &mut actions, parent_ix, child_ix);
        }
    }
    // TODO when joining project away any vars that are not in other chunks, in other primitives or in the select
    while primitives.len() > 0 {
        let (primitive_ix, subtree) = cheapest_primitive_subtree(&chunks, &join_tree, &primitives);
        let root_ix = collapse_subtree(&mut chunks, &mut actions, &mut join_tree, &query.select, &primitives, &subtree);
        apply(&mut chunks, &mut actions, strings, root_ix, &primitives[primitive_ix]);
        primitives.remove(primitive_ix);
    }
    let remaining_tree = join_tree.clone();
    let root_ix = collapse_subtree(&mut chunks, &mut actions, &mut join_tree, &query.select, &primitives, &remaining_tree);
    sort_and_project(&mut chunks, &mut actions, root_ix, &query.select, &HashSet::new());
    runtime::Query{upstream: upstream, actions: actions, result_ix: root_ix}
}

pub fn compile_input(input: &Input, schema: &[Kind], strings: &mut Vec<String>) -> runtime::Chunk {
    let mut data = vec![];
    if let Some((ref filename, ref columns)) = input.tsv {
        let mut contents = String::new();
        File::open(filename).unwrap().read_to_string(&mut contents).unwrap();
        let mut lines = contents.lines();
        lines.next(); // drop header
        for line in lines {
            let fields = line.split("\t").collect::<Vec<_>>();
            for (kind, column) in schema.iter().zip(columns.iter()) {
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
    }
    for row in input.rows.iter() {
        assert_eq!(row.len(), schema.len());
        for (value, kind) in row.iter().zip(schema.iter()) {
            match (value, *kind) {
                (&Value::Id(id), Kind::Id) => data.push(id),
                (&Value::Number(number), Kind::Number) => data.push(number as u64),
                (&Value::Text(ref text), Kind::Text) => {
                    let text = text.to_owned();
                    data.push(hash(&text));
                    data.push(strings.len() as u64);
                    strings.push(text);
                }
                _ => panic!("Kind mismatch: {:?} {:?}", kind, value),
            }
        }
    }
    let row_width = schema.iter().map(|kind| kind.width()).sum();
    runtime::Chunk{data: data, row_width: row_width}
}

pub fn compile_union(union: &Union, schema: &[Kind], program: &Program) -> runtime::Union {
    let upstream = union.members.iter().map(|member| {
        match *member {
            Member::Insert(ref member_id) => program.ids.iter().position(|id| id == member_id).unwrap(),
            Member::Remove(ref member_id) => program.ids.iter().position(|id| id == member_id).unwrap(),
        }
    }).collect();
    let members = union.members.iter().map(|member| {
        match *member {
            Member::Insert(_) => runtime::Member::Insert,
            Member::Remove(_) => runtime::Member::Remove,
        }
    }).collect();
    let key = (0..schema.len()).map(|ix| {
        schema[0..ix].iter().map(|kind| kind.width()).sum()
    }).collect();
    runtime::Union{upstream: upstream, members: members, key: key}
}

pub fn compile(program: &Program) -> runtime::Program {
    let mut ids = vec![];
    let mut schemas = vec![];
    let mut states = vec![];
    let mut views = vec![];
    let mut downstreams = vec![];
    let mut dirty = vec![];
    let mut strings = vec![];

    let mut scheduled_ixes = program.schedule.iter().zip(0..program.ids.len()).collect::<Vec<_>>();
    scheduled_ixes.sort();
    for (_, program_ix) in scheduled_ixes {
        ids.push(hash(&program.ids[program_ix]));
        let schema = program.schemas[program_ix].clone();
        match program.views[program_ix] {
            View::Input(ref input) => {
                let state = compile_input(input, &schema[..], &mut strings);
                states.push(Rc::new(state));
                views.push(runtime::View::Input);
                dirty.push(false);
            }
            View::Query(ref query) => {
                let runtime_query = compile_query(query, program, &mut strings);
                let row_width = schema.iter().map(|kind| kind.width()).sum();
                states.push(Rc::new(runtime::Chunk{data: vec![], row_width: row_width}));
                views.push(runtime::View::Query(runtime_query));
                dirty.push(true);
            }
            View::Union(ref union) => {
                let runtime_union = compile_union(union, &schema[..], program);
                let row_width = schema.iter().map(|kind| kind.width()).sum();
                states.push(Rc::new(runtime::Chunk{data: vec![], row_width: row_width}));
                views.push(runtime::View::Union(runtime_union));
                dirty.push(true);
            }
        }
        schemas.push(schema);
        downstreams.push(vec![]);
    }

    for (schedule_ix, view) in views.iter().enumerate() {
        match *view {
            runtime::View::Query(ref query) => {
                for upstream_ix in query.upstream.iter() {
                    downstreams[*upstream_ix].push(schedule_ix);
                }
            }
            runtime::View::Union(ref union) => {
                for upstream_ix in union.upstream.iter() {
                    downstreams[*upstream_ix].push(schedule_ix);
                }
            }
            runtime::View::Input => (),
        }
    }

    runtime::Program{ids: ids, schemas: schemas, states: states, views: views, downstreams: downstreams, dirty: dirty, strings: strings}
}

// We shall see that at which dogs howl in the dark, and that at which cats prick up their ears after midnight
fn parse_clause(text: &str) -> (ViewId, Vec<Binding>, Vec<Option<Kind>>) {
    let var_re = Regex::new(r#"_|\?[:alnum:]*(:[:alnum:]*)?|"[^"]*"|([:digit:]|\.)+|#[:digit:]+"#).unwrap();
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

// I have seen beyond the bounds of infinity and drawn down daemons from the stars
fn parse_input(lines: Vec<&str>, view_id: ViewId, schema: Vec<Kind>) -> Vec<(ViewId, Vec<Kind>, View)> {
    let mut words = lines[1].split(" ");
    words.next().unwrap(); // drop "="
    let tsv = words.next().map(|filename| {
        let columns = words.map(|word| word.parse::<usize>().unwrap()).collect();
        (filename.to_owned(), columns)
    });
    let value_re = Regex::new(r#""[^"]*"|([:digit:]|\.)+|#[:digit:]+"#).unwrap();
    let rows = lines[2..].iter().map(|line| {
        let values = line.matches(&value_re).map(|value_text| {
            match value_text.chars().next().unwrap() {
                '#' => Value::Id(value_text[1..].parse::<u64>().unwrap()),
                '"' => Value::Text(value_text[1..value_text.len()-1].to_owned()),
                _ => Value::Number(value_text.parse::<f64>().unwrap()),
            }
        }).collect::<Vec<_>>();
        assert_eq!(values.len(), schema.len());
        values
    }).collect();
    vec![(view_id, schema, View::Input(Input{tsv: tsv, rows: rows}))]
}

// I have harnessed the shadows that stride from world to world to sow death and madness
fn parse_query(mut lines: Vec<&str>, view_id: ViewId, schema: Vec<Kind>, select: Vec<VariableId>) -> Vec<(ViewId, Vec<Kind>, View)> {
    let mut members = vec![];
    let mut views = vec![];
    let mut clauses = vec![];
    lines.remove(0); // drop header
    for line in lines.iter() {
        match *line {
            "+" => {
                let member_id = format!("{} | member {}", view_id, members.len());
                let query_id = format!("{} | member {}", view_id, (members.len() as isize) - 1);
                members.push(Member::Insert(member_id));
                let query = View::Query(Query{clauses: clauses, select: select.clone()});
                views.push((query_id, schema.clone(), query));
                clauses = vec![];
            }
            "-" => {
                let member_id = format!("{} | member {}", view_id, members.len());
                let query_id = format!("{} | member {}", view_id, (members.len() as isize) - 1);
                members.push(Member::Remove(member_id));
                let query = View::Query(Query{clauses: clauses, select: select.clone()});
                views.push((query_id, schema.clone(), query));
                clauses = vec![];
            }
            _ => {
                let (view_id, bindings, kinds) = parse_clause(line);
                for kind in kinds.into_iter() {
                    assert_eq!(kind, None);
                }
                clauses.push(Clause{view: view_id, bindings: bindings})
            }
        }
    }
    let query_id = format!("{} | member {}", view_id, members.len() - 1);
    let query = View::Query(Query{clauses: clauses, select: select.clone()});
    views.push((query_id, schema.clone(), query));
    views.remove(0);
    views.push((view_id, schema.clone(), View::Union(Union{members: members})));
    views
}

// If I am mad, it is mercy!
fn parse_view(text: &str) -> Vec<(ViewId, Vec<Kind>, View)> {
    let lines = text.split("\n").collect::<Vec<_>>();
    let (view_id, bindings, kinds) = parse_clause(lines[0]);
    let select = bindings.into_iter().map(|binding| match binding { Binding::Variable(var) => var, _ => panic!() }).collect::<Vec<_>>();
    let schema = kinds.into_iter().map(|kind| kind.unwrap()).collect::<Vec<_>>();
    match lines[1].chars().next().unwrap() {
        '=' => parse_input(lines, view_id, schema),
        '+' => parse_query(lines, view_id, schema, select),
        '-' => parse_query(lines, view_id, schema, select),
        _ => panic!("What are this? {:?}", lines[1]),
    }
}

pub fn parse(text: &str) -> Program {
    let mut ids = vec![];
    let mut schedule = vec![];
    let mut schemas = vec![];
    let mut views = vec![];
    for view_text in text.split("\n\n") {
        if view_text != "" {
            for (id, schema, view) in parse_view(view_text) {
                schedule.push(ids.len()); // ie just scheduling in textual order for now
                ids.push(id);
                schemas.push(schema);
                views.push(view);
            }
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
    parse(&text[..])
}

#[cfg(test)]
pub mod tests{
    use super::*;
    use test::{Bencher, black_box};

    #[test]
    pub fn test_metal() {
        let bootstrap_program = load(&["data/chinook.imp", "data/metal.imp"]);
        let mut runtime_program = compile(&bootstrap_program);
        runtime_program.run();
        assert_set_eq!(
            runtime_program.states[5].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["AC/DC", "Accept", "Black Sabbath", "Metallica", "Iron Maiden", "Mot\u{f6}rhead", "M\u{f6}tley Cr\u{fc}e", "Ozzy Osbourne", "Scorpions"]
            );
    }

    #[bench]
    pub fn bench_metal_run(bencher: &mut Bencher) {
        let bootstrap_program = load(&["data/chinook.imp", "data/metal.imp"]);
        let runtime_program = compile(&bootstrap_program);
        bencher.iter(|| {
            let mut runtime_program = runtime_program.clone();
            runtime_program.run();
            black_box(&runtime_program.states[5]);
        });
    }

    #[bench]
    pub fn bench_metal_all(bencher: &mut Bencher) {
        bencher.iter(|| {
            let bootstrap_program = load(&["data/chinook.imp", "data/metal.imp"]);
            let mut runtime_program = compile(&bootstrap_program);
            runtime_program.run();
            black_box(&runtime_program.states[5]);
        });
    }

    #[test]
    pub fn test_selfjoin() {
        let bootstrap_program = load(&["data/chinook.imp", "data/selfjoin.imp"]);
        let mut runtime_program = compile(&bootstrap_program);
        runtime_program.run();
        assert_set_eq!(
            runtime_program.states[5].data.iter().cloned(),
            vec![1,5,8]
            );
    }

    #[test]
    pub fn test_paths() {
        let bootstrap_program = load(&["data/paths.imp"]);
        let mut runtime_program = compile(&bootstrap_program);
        runtime_program.run();
        assert_eq!(
            runtime_program.states[3].data,
            vec![0, 1, 0, 2, 0, 3, 0, 4, 1, 1, 1, 2, 1, 3, 1, 4, 2, 1, 2, 2, 2, 3, 2, 4, 3, 1, 3, 2, 3, 3, 3, 4]
            );
    }

    #[test]
    pub fn test_flying() {
        let bootstrap_program = load(&["data/flying.imp"]);
        let mut runtime_program = compile(&bootstrap_program);
        runtime_program.run();
        assert_set_eq!(
            runtime_program.states[6].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["Sally the sparrow", "Ellen the eagle", "Harry the penguin"]
            );
    }

    #[test]
    pub fn test_math() {
        let bootstrap_program = load(&["data/math.imp"]);
        let mut runtime_program = compile(&bootstrap_program);
        runtime_program.run();
        assert_set_eq!(
            runtime_program.states[2].data.iter().cloned(),
            vec![3,4,6,8, 6,7,9,11, 11,12,14,16]
            );
        assert_set_eq!(
            runtime_program.states[4].data.iter().cloned(),
            vec![3,4,6,8]
            );
    }
}