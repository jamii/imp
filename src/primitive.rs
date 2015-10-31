use std::cmp::Ordering;

use runtime::{Chunk, Kind, Direction, from_number, to_number};
use bootstrap::{self, Binding, bound_vars, PrimitiveOrNegated};

#[derive(Clone, Debug, Copy)]
pub enum Primitive {
    Add,
    Sum,
    Ordinal,
    LessThan,
    Copy,
    Min,
}

// TODO this is grossly inefficient compared to untyped sort
fn typed_sort(chunk: &Chunk, ixes: &[(usize, Kind, Direction)], strings: &Vec<String>) -> Chunk {
    let mut data = chunk.data.clone();
    for &(ix, kind, direction) in ixes.iter().rev() {
        let mut new_data = Vec::with_capacity(data.len());
        match kind {
            Kind::Id => {
                let mut buffer = Vec::with_capacity(data.len() / chunk.row_width);
                for row in data.chunks(chunk.row_width) {
                    buffer.push((row[ix], row));
                }
                match direction {
                    Direction::Ascending => buffer.sort_by(|&(key_a, _), &(key_b, _)| key_a.cmp(&key_b)),
                    Direction::Descending => buffer.sort_by(|&(key_a, _), &(key_b, _)| key_b.cmp(&key_a)),
                }
                for (_, row) in buffer.into_iter() {
                    new_data.extend(row);
                }
            }
            Kind::Number => {
                let mut buffer = Vec::with_capacity(data.len() / chunk.row_width);
                for row in data.chunks(chunk.row_width) {
                    buffer.push((to_number(row[ix]), row));
                }
                // TODO NaN can cause panic here
                match direction {
                    Direction::Ascending => buffer.sort_by(|&(key_a, _), &(key_b, _)| key_a.partial_cmp(&key_b).unwrap()),
                    Direction::Descending => buffer.sort_by(|&(key_a, _), &(key_b, _)| key_b.partial_cmp(&key_a).unwrap()),
                }
                for (_, row) in buffer.into_iter() {
                    new_data.extend(row);
                }
            }
            Kind::Text => {
                let mut buffer = Vec::with_capacity(data.len() / chunk.row_width);
                for row in data.chunks(chunk.row_width) {
                    buffer.push((&strings[row[ix+1] as usize], row));
                }
                match direction {
                    Direction::Ascending => buffer.sort_by(|&(ref key_a, _), &(ref key_b, _)| key_a.cmp(key_b)),
                    Direction::Descending => buffer.sort_by(|&(ref key_a, _), &(ref key_b, _)| key_b.cmp(key_a)),
                }
                for (_, row) in buffer.into_iter() {
                    new_data.extend(row);
                }
            }
        }
        data = new_data;
    }
    Chunk{data: data, row_width: chunk.row_width}
}

fn typed_cmp(row_a: &[u64], row_b: &[u64], ixes: &[(usize, Kind, Direction)], strings: &Vec<String>) -> Ordering {
    for &(ix, kind, direction) in ixes.iter() {
        let ordering = match kind {
            Kind::Id => row_a[ix].cmp(&row_b[ix]),
            // TODO NaN can cause panic here
            Kind::Number => to_number(row_a[ix]).partial_cmp(&to_number(row_b[ix])).unwrap(),
            Kind::Text => strings[row_a[ix+1] as usize].cmp(&strings[row_b[ix+1] as usize]),
        };
        match (ordering, direction) {
            (Ordering::Greater, Direction::Ascending) => return Ordering::Greater,
            (Ordering::Less, Direction::Ascending) => return Ordering::Less,
            (Ordering::Greater, Direction::Descending) => return Ordering::Less,
            (Ordering::Less, Direction::Descending) => return Ordering::Greater,
            (Ordering::Equal, _) => (),
        }
    }
    return Ordering::Equal;
}

impl Primitive {
    pub fn apply(&self, chunk: &Chunk, input_ixes: &[usize], group_ixes: &[usize], over_ixes: &[(usize, Kind, Direction)], strings: &Vec<String>) -> Chunk {
        let mut data = vec![];
        match (*self, input_ixes) {
            (Primitive::Add, [a, b]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    data.extend(row);
                    data.push(from_number(to_number(row[a]) + to_number(row[b])));
                }
            }
            (Primitive::Sum, [a]) => {
                let sorted_chunk = chunk.sort(group_ixes);
                for group in sorted_chunk.groups(group_ixes) {
                    let mut sum = 0f64;
                    for row in group.chunks(chunk.row_width) {
                        sum += to_number(row[a]);
                    }
                    for row in group.chunks(chunk.row_width) {
                        data.extend(row);
                        data.push(from_number(sum));
                    }
                }
            }
            (Primitive::Ordinal, []) => {
                let sorted_chunk = typed_sort(chunk, over_ixes, strings).sort(group_ixes);
                for group in sorted_chunk.groups(group_ixes) {
                    for (ordinal, row) in group.chunks(chunk.row_width).enumerate() {
                        data.extend(row);
                        data.push(from_number((ordinal + 1) as f64));
                    }
                }
            }
            (Primitive::LessThan, [a, b]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    if to_number(row[a]) < to_number(row[b]) {
                        data.extend(row);
                    }
                }
            }
            (Primitive::Copy, [a]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    data.extend(row);
                    data.push(row[a]);
                }
            }
            (Primitive::Min, []) => {
                let sorted_chunk = chunk.sort(group_ixes);
                for group in sorted_chunk.groups(group_ixes) {
                    let mut min = &group[0..chunk.row_width];
                    for row in group[chunk.row_width..].chunks(chunk.row_width) {
                        match typed_cmp(min, row, over_ixes, strings) {
                            Ordering::Greater => min = row,
                            _ => ()
                        }
                    }
                    data.extend(min);
                }
            }
            _ => panic!("What are this: {:?} {:?} {:?}", self, input_ixes, group_ixes)
        }
        let num_outputs = match *self {
            Primitive::Add => 1,
            Primitive::Sum => 1,
            Primitive::Ordinal => 1,
            Primitive::LessThan => 0,
            Primitive::Copy => 1,
            Primitive::Min => 0,
        };
        Chunk{data: data, row_width: chunk.row_width + num_outputs}
    }
}

pub fn for_bootstrap(program: &bootstrap::Program, view_id: &str, bindings: &Vec<Binding>, over_bindings: &Vec<(Binding, Direction)>) -> Option<bootstrap::Primitive> {
    use self::Primitive::*;
    use runtime::Kind::*;
    let bound_over_vars = bound_vars(&over_bindings.iter().map(|&(ref binding, _)| binding).cloned().collect());
    match (view_id, &bindings[..]) {
        ("_ = _ + _", [ref a, ref b, ref c]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Add),
            input_kinds: vec![Number, Number],
            input_bindings: vec![b.clone(), c.clone()],
            output_kinds: vec![Number],
            output_bindings: vec![a.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![b.clone(), c.clone()]),
            bound_output_vars: bound_vars(&vec![a.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ = sum(_)", [ref a, ref b]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Sum),
            input_kinds: vec![Number],
            input_bindings: vec![b.clone()],
            output_kinds: vec![Number],
            output_bindings: vec![a.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![b.clone()]),
            bound_output_vars: bound_vars(&vec![a.clone()]),
            bound_aggregate_vars: &bound_vars(&vec![b.clone()]) | &bound_over_vars,
        }),
        ("row _", [ref a]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Ordinal),
            input_kinds: vec![],
            input_bindings: vec![],
            output_kinds: vec![Number],
            output_bindings: vec![a.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![]),
            bound_output_vars: bound_vars(&vec![a.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ < _", [ref a, ref b]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(LessThan),
            input_kinds: vec![Number, Number],
            input_bindings: vec![a.clone(), b.clone()],
            output_kinds: vec![],
            output_bindings: vec![],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![a.clone(), b.clone()]),
            bound_output_vars: bound_vars(&vec![]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ <- _", [ref a, ref b]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Copy),
            input_kinds: vec![Id],
            input_bindings: vec![b.clone()],
            output_kinds: vec![Id],
            output_bindings: vec![a.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![b.clone()]),
            bound_output_vars: bound_vars(&vec![a.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("min", []) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Min),
            input_kinds: vec![],
            input_bindings: vec![],
            output_kinds: vec![],
            output_bindings: vec![],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![]),
            bound_output_vars: bound_vars(&vec![]),
            bound_aggregate_vars: bound_over_vars,
        }),
        _ => {
            if view_id.starts_with("! ") {
                let ix = program.ids.iter().position(|id| *id == view_id[2..]).unwrap();
                Some(bootstrap::Primitive{
                    primitive: PrimitiveOrNegated::Negated(ix),
                    input_kinds: program.schemas[ix].clone(),
                    input_bindings: bindings.clone(),
                    output_kinds: vec![],
                    output_bindings: vec![],
                    over_bindings: vec![],
                    bound_input_vars: bound_vars(bindings),
                    bound_output_vars: bound_vars(&vec![]),
                    bound_aggregate_vars: bound_vars(&vec![]),
                })
            } else {
                None
            }
        }
    }
}