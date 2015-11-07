use std::collections::HashSet;
use regex::Regex;
use std::path::Path;
use std::io::prelude::*;
use std::fs::File;
use std::rc::Rc;

use primitive;
use runtime::{self, hash, Kind, push_string};

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
    pub over_bindings: Vec<(Binding, runtime::Direction)>,
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
pub enum PrimitiveOrNegated {
    Primitive(primitive::Primitive),
    Negated(usize),
}

#[derive(Clone, Debug)]
pub struct Primitive {
    pub primitive: PrimitiveOrNegated,
    pub input_kinds: Vec<Kind>,
    pub output_kinds: Vec<Kind>,
    pub input_bindings: Vec<Binding>,
    pub output_bindings: Vec<Binding>,
    pub over_bindings: Vec<(Binding, runtime::Direction)>,
    pub bound_input_vars: HashSet<VariableId>,
    pub bound_output_vars: HashSet<VariableId>,
    pub bound_aggregate_vars: HashSet<VariableId>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Binding {
    Unbound,
    Constant(Value),
    Variable(VariableId),
}

pub type KindedBinding = (Binding, Option<Kind>);

#[derive(Clone, Debug, PartialEq)]
pub enum Word {
    View(String),
    KindedBinding(KindedBinding)
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

pub fn bound_vars(bindings: &Vec<Binding>) -> HashSet<VariableId> {
    let mut vars = HashSet::new();
    for binding in bindings.iter() {
        match *binding {
            Binding::Variable(ref var) => vars.insert(var.clone()),
            _ => false,
        };
    }
    vars
}

fn find_join_ear(chunks: &Vec<Chunk>, to_join: &Vec<usize>) -> (usize, usize) {
    for &child_ix in to_join.iter() {
        let child_vars = &chunks[child_ix].bound_vars;
        let mut joined_vars = HashSet::new();
        for &other_ix in to_join.iter() {
            if child_ix != other_ix {
                let other_vars = &chunks[other_ix].bound_vars;
                joined_vars.extend(child_vars.intersection(other_vars).cloned());
            }
        }
        for &parent_ix in to_join.iter() {
            if child_ix != parent_ix {
                let parent_vars = &chunks[parent_ix].bound_vars;
                if joined_vars.is_subset(parent_vars) {
                    return (child_ix, parent_ix);
                }
            }
        }
    }
    panic!("Cant find an ear in: {:#?}", (chunks, to_join));
}

fn build_join_tree(chunks: &Vec<Chunk>, mut to_join: Vec<usize>) -> Tree {
    assert!(chunks.len() > 0);
    assert!(to_join.len() > 0);
    let mut tree = vec![];
    while to_join.len() > 1 { // one chunk will be left behind as the root
        let (child_ix, parent_ix) = find_join_ear(chunks, &to_join);
        to_join.retain(|ix| *ix != child_ix);
        tree.push((child_ix, Some(parent_ix)));
    }
    tree.push((to_join[0], None));
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

fn vars_for_subtree(chunks: &Vec<Chunk>, tree: &Tree, subtree: &Tree) -> (HashSet<VariableId>, HashSet<VariableId>) {
    let mut vars_inside = HashSet::new();
    let mut vars_outside = HashSet::new();
    for &(child_ix, _) in tree.iter() {
        if subtree.iter().any(|&(subtree_child_ix, _)| subtree_child_ix == child_ix) {
            vars_inside.extend(chunks[child_ix].bound_vars.clone());
        } else {
            vars_outside.extend(chunks[child_ix].bound_vars.clone());
        }
    }
    (vars_inside, vars_outside)
}

fn cheapest_primitive_subtree(chunks: &Vec<Chunk>, join_tree: &Tree, primitives: &Vec<Primitive>) -> (usize, Tree) {
    for subtree in all_subtrees(join_tree).into_iter() {
        let (vars_inside, vars_outside) = vars_for_subtree(chunks, join_tree, &subtree);
        for (primitive_ix, primitive) in primitives.iter().enumerate() {
            if primitive.bound_input_vars.is_subset(&vars_inside)
            && (primitive.bound_aggregate_vars.intersection(&vars_outside).count() == 0) {
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
                Value::Number(number) => runtime::from_number(number),
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
                            constants.push(runtime::from_number(number));
                            chunk.kinds.push(Kind::Number);
                            chunk.bindings.push(Binding::Unbound);
                        }
                        Value::Text(ref string) => {
                            runtime::push_string(&mut constants, strings, string.to_owned());
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
        // TODO will eventually need to maintain the order of over_bindings
        let group_ixes = chunk.bound_vars.difference(&primitive.bound_aggregate_vars).map(|var| {
            let ix = chunk.bindings.iter().position(|binding| {
                match *binding {
                    Binding::Variable(ref bound_var) => bound_var == var,
                    _ => false
                }
            }).unwrap();
            chunk.kinds.iter().take(ix).map(|kind| kind.width()).sum()
        }).collect();
        let over_ixes = primitive.over_bindings.iter().map(|&(ref binding, direction)| {
            let ix = chunk.bindings.iter().position(|chunk_binding| chunk_binding == binding).unwrap();
            let column = chunk.kinds.iter().take(ix).map(|kind| kind.width()).sum();
            let kind = chunk.kinds[ix];
            (column, kind, direction)
        }).collect();
        match primitive.primitive {
            PrimitiveOrNegated::Primitive(runtime_primitive) => {
                actions.push(runtime::Action::Apply(chunk_ix, runtime_primitive, input_ixes, group_ixes, over_ixes));
            }
            _ => unreachable!(),
        }
        chunk.kinds.extend(primitive.output_kinds.clone());
        chunk.bindings.extend(primitive.output_bindings.clone());
        chunk.bound_vars.extend(primitive.bound_output_vars.clone());
    }
    filter(chunks, actions, chunk_ix); // handle any output vars that are constants
    selfjoin(chunks, actions, chunk_ix); // handle any output vars that need joining
}

pub fn negate(chunks: &mut Vec<Chunk>, actions: &mut Vec<runtime::Action>, chunk_ix: usize, primitive: &Primitive) {
    match primitive.primitive {
        PrimitiveOrNegated::Negated(negated_ix) => {
            let join_vars = chunks[negated_ix].bound_vars.intersection(&chunks[chunk_ix].bound_vars).cloned().collect();
            let chunk_vars = &chunks[chunk_ix].bound_vars.clone();
            sort_and_project(chunks, actions, negated_ix, &join_vars, &HashSet::new());
            sort_and_project(chunks, actions, chunk_ix, &join_vars, chunk_vars);
            let left_key = sort_key(&chunks[negated_ix], &join_vars);
            let right_key = sort_key(&chunks[chunk_ix], &join_vars);
            assert_eq!(left_key.len(), right_key.len());
            actions.push(runtime::Action::AntiJoin(negated_ix, chunk_ix, left_key, right_key));
        }
        _ => unreachable!(),
    }
}

pub fn compile_query(query: &Query, program: &Program, strings: &mut Vec<String>) -> runtime::Query {
    let mut upstream = vec![];
    let mut chunks = vec![];
    let mut primitives = vec![];
    for clause in query.clauses.iter() {
        let view = program.ids.iter().position(|id| *id == clause.view);
        let primitive = primitive::for_bootstrap(&program, &clause.view, &clause.bindings, &clause.over_bindings);
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
    let to_join = (0..chunks.len()).collect();
    for primitive in primitives.iter_mut() {
        let Primitive{primitive: ref mut primitive_or_negated, input_kinds: ref kinds, input_bindings: ref bindings, ..} = *primitive;
        // TODO this is horrific hackery
        match *primitive_or_negated {
            PrimitiveOrNegated::Negated(ref mut ix) => {
                upstream.push(*ix);
                *ix = chunks.len();
                chunks.push(Chunk{
                    kinds: kinds.clone(),
                    bindings: bindings.clone(),
                    bound_vars: bound_vars(bindings),
                });
            }
            _ => (),
        }
    }
    let mut join_tree = build_join_tree(&chunks, to_join);
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
    for edge in join_tree.iter() {
        if let &(child_ix, Some(parent_ix)) = edge {
            semijoin(&mut chunks, &mut actions, parent_ix, child_ix);
        }
    }
    while primitives.len() > 0 {
        let (primitive_ix, subtree) = cheapest_primitive_subtree(&chunks, &join_tree, &primitives);
        let root_ix = collapse_subtree(&mut chunks, &mut actions, &mut join_tree, &query.select, &primitives, &subtree);
        {
            let primitive = &primitives[primitive_ix];
            assert!(primitive.bound_input_vars.is_subset(&chunks[root_ix].bound_vars));
            match primitive.primitive {
                PrimitiveOrNegated::Primitive(_) => apply(&mut chunks, &mut actions, strings, root_ix, primitive),
                PrimitiveOrNegated::Negated(_) => negate(&mut chunks, &mut actions, root_ix, primitive),
            }
        }
        primitives.remove(primitive_ix);
    }
    let remaining_tree = join_tree.clone();
    let root_ix = collapse_subtree(&mut chunks, &mut actions, &mut join_tree, &query.select, &primitives, &remaining_tree);
    sort_and_project(&mut chunks, &mut actions, root_ix, &query.select, &HashSet::new());
    assert_eq!(&query.select.iter().cloned().collect::<HashSet<_>>(), &chunks[root_ix].bound_vars);
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
                    Kind::Number => data.push(runtime::from_number(fields[*column].parse::<f64>().unwrap())),
                    Kind::Text => push_string(&mut data, strings, fields[*column].to_owned()),
                }
            }
        }
    }
    for row in input.rows.iter() {
        assert_eq!(row.len(), schema.len());
        for (value, kind) in row.iter().zip(schema.iter()) {
            match (value, *kind) {
                (&Value::Id(id), Kind::Id) => data.push(id),
                (&Value::Number(number), Kind::Number) => data.push(runtime::from_number(number)),
                (&Value::Text(ref text), Kind::Text) => push_string(&mut data, strings, text.to_owned()),
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

peg_file! parse("parse.rustpeg");

fn parse_clause(text: &str) -> (ViewId, Vec<Binding>, Vec<Option<Kind>>, Vec<(Binding, runtime::Direction)>) {
    let (words, over_bindings) = parse::clause(text).unwrap();
    let mut bindings = vec![];
    let mut kinds = vec![];
    let view_id_words:Vec<String> = words.iter().map(|word| {
        match word.to_owned() {
            Word::View(vs) => vs,
            Word::KindedBinding((b, k)) => {
                bindings.push(b);
                kinds.push(k);
                "_".to_owned()
            }
        }
    }).collect();
    let view_id = view_id_words.join("");
    (view_id, bindings, kinds, over_bindings)
}

fn parse_input(lines: Vec<&str>, view_id: ViewId, schema: Vec<Kind>) -> Vec<(ViewId, Vec<Kind>, View)> {
    let tsv = parse::input_tsv(lines[1]).unwrap();
    let rows = lines[2..].iter().map(|line| {
        let values = parse::input_row(line).unwrap();
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
                let (view_id, bindings, kinds, over_bindings) = parse_clause(line);
                for kind in kinds.into_iter() {
                    assert_eq!(kind, None);
                }
                clauses.push(Clause{view: view_id, bindings: bindings, over_bindings: over_bindings})
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
    let (view_id, bindings, kinds, over_bindings) = parse_clause(lines[0]);
    assert_eq!(over_bindings, vec![]);
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
    use runtime::{self, to_number, from_number};
    use test::{Bencher, black_box};
    use std::io::prelude::*;
    use std::fs::File;
    use std::rc::Rc;

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
            runtime_program.states[2].data.iter().map(|d| to_number(*d)),
            vec![3.0,4.0,6.0,8.0, 6.0,7.0,9.0,11.0, 11.0,12.0,14.0,16.0]
            );
        assert_set_eq!(
            runtime_program.states[4].data.iter().map(|d| to_number(*d)),
            vec![3.0,4.0,6.0,8.0]
            );
        assert_set_eq!(
            runtime_program.states[6].data.iter().map(|d| to_number(*d)),
            vec![17.0]
            );
        assert_set_eq!(
            runtime_program.states[9].data.chunks(2).map(|chunk|
                (to_number(chunk[0]), to_number(chunk[1]))),
            vec![(1.5, 3.0), (2.0, 4.0), (2.7, 5.4)]
            );
        assert_set_eq!(
            runtime_program.states[12].data.chunks(3).map(|chunk|
                (&runtime_program.strings[chunk[1] as usize][..], to_number(chunk[2]))),
            vec![("alice", 100.0), ("bob", 50.0), ("eve", 200.0)]
            );
        assert_set_eq!(
            runtime_program.states[14].data.chunks(3).map(|chunk|
                (&runtime_program.strings[chunk[1] as usize][..], to_number(chunk[2]))),
            vec![("alice corp", 250.0), ("evil eve studios", 100.0)]
            );
        assert_set_eq!(
            runtime_program.states[16].data.iter().map(|d| to_number(*d)),
            vec![350.0]
            );
    }

    #[test]
    pub fn test_sorting() {
        let bootstrap_program = load(&["data/sorting.imp"]);
        let mut runtime_program = compile(&bootstrap_program);
        runtime_program.run();
        assert_set_eq!(
            runtime_program.states[2].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["bob", "eve"]
            );
        assert_set_eq!(
            runtime_program.states[4].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["bob"]
            );
        assert_set_eq!(
            runtime_program.states[6].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["alice", "eve"]
            );
        assert_set_eq!(
            runtime_program.states[8].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["alice"]
            );
        assert_set_eq!(
            runtime_program.states[10].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["eve"]
            );
    }

    #[test]
    pub fn test_negation() {
        let bootstrap_program = load(&["data/negation.imp"]);
        let mut runtime_program = compile(&bootstrap_program);
        runtime_program.run();
        assert_set_eq!(
            runtime_program.states[4].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["alice", "bob", "cin"]
            );
        assert_set_eq!(
            runtime_program.states[6].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["eve", "alice", "bob"]
            );
        assert_set_eq!(
            runtime_program.states[8].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["alice", "bob", "cin"]
            );
    }

    #[test]
    pub fn test_strings() {
        let bootstrap_program = load(&["data/strings.imp"]);
        let mut runtime_program = compile(&bootstrap_program);
        runtime_program.run();
        assert_set_eq!(
            runtime_program.states[2].data.chunks(3).map(|chunk| (&runtime_program.strings[chunk[1] as usize][..], to_number(chunk[2]))),
            vec![("apple", 0.0), ("cake", 1.0), ("swordfish", 2.0)]
            );
        assert_set_eq!(
            runtime_program.states[4].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["apple, cake, swordfish"]
            );
        assert_set_eq!(
            runtime_program.states[6].data.chunks(2).map(|chunk| &runtime_program.strings[chunk[1] as usize][..]),
            vec!["apple"]
            );
    }

    #[test]
    pub fn test_imp() {
        let bootstrap_program = load(&["data/imp.imp"]);
        let mut runtime_program = compile(&bootstrap_program);
        let mut text = String::new();
        File::open("data/imp.imp").unwrap().read_to_string(&mut text).unwrap();
        {
            let runtime::Program{ref mut states, ref mut strings, ..} = runtime_program;
            let mut data = vec![];
            runtime::push_string(&mut data, strings, "program".to_owned());
            data.push(from_number(0.0));
            data.push(from_number(text.len() as f64));
            runtime::push_string(&mut data, strings, "program".to_owned());
            data.push(from_number(0.0));
            data.push(from_number(text.len() as f64));
            runtime::push_string(&mut data, strings, text);
            let mut chunk = (*states[0]).clone();
            chunk.data = data;
            states[0] = Rc::new(chunk);
        }
        runtime_program.run();
        runtime_program.print();
        assert!(false);
    }
}