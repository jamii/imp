use std::cmp::Ordering;
use regex::{Regex};
use std::collections::HashMap;

use runtime::{Chunk, Kind, Direction, from_number, to_number, push_string};
use bootstrap::{self, Binding, bound_vars, PrimitiveOrNegated};

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

#[derive(Clone, Debug, Copy)]
pub enum Primitive {
    Add,
    Sum,
    Ordinal,
    LessThan,
    Copy,
    CopyString,
    Min,
    Split,
    Search,
    Capture,
    Replace,
    Substring,
    Length,
    IsNumber,
    IsId,
    NotEqual,
    Count,
    GetBit,
    SetBit,
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
        ("_ <<- _", [ref a, ref b]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(CopyString),
            input_kinds: vec![Text],
            input_bindings: vec![b.clone()],
            output_kinds: vec![Text],
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
        ("result _ of _ split by _ is at _ to _ breaking at _", [ref result_ix, ref text, ref regex, ref from_ix, ref to_ix, ref break_ix]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Split),
            input_kinds: vec![Text, Text],
            input_bindings: vec![text.clone(), regex.clone()],
            output_kinds: vec![Number, Number, Number, Number],
            output_bindings: vec![result_ix.clone(), from_ix.clone(), to_ix.clone(), break_ix.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![text.clone(), regex.clone()]),
            bound_output_vars: bound_vars(&vec![result_ix.clone(), from_ix.clone(), to_ix.clone(), break_ix.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("result _ of _ searched by _ is at _ to _", [ref result_ix, ref text, ref regex, ref from_ix, ref to_ix]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Search),
            input_kinds: vec![Text, Text],
            input_bindings: vec![text.clone(), regex.clone()],
            output_kinds: vec![Number, Number, Number],
            output_bindings: vec![result_ix.clone(), from_ix.clone(), to_ix.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![text.clone(), regex.clone()]),
            bound_output_vars: bound_vars(&vec![result_ix.clone(), from_ix.clone(), to_ix.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("capture _ of result _ of _ searched by _ is at _ to _", [ref capture_ix, ref result_ix, ref text, ref regex, ref from_ix, ref to_ix]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Capture),
            input_kinds: vec![Text, Text],
            input_bindings: vec![text.clone(), regex.clone()],
            output_kinds: vec![Number, Number, Number, Number],
            output_bindings: vec![capture_ix.clone(), result_ix.clone(), from_ix.clone(), to_ix.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![text.clone(), regex.clone()]),
            bound_output_vars: bound_vars(&vec![capture_ix.clone(), result_ix.clone(), from_ix.clone(), to_ix.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ with _ replaced by _ is _", [ref text, ref regex, ref replacement, ref result]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Replace),
            input_kinds: vec![Text, Text, Text],
            input_bindings: vec![text.clone(), regex.clone(), replacement.clone()],
            output_kinds: vec![Text],
            output_bindings: vec![result.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![text.clone(), regex.clone(), replacement.clone()]),
            bound_output_vars: bound_vars(&vec![result.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("the text at _ to _ in _ is _", [ref from_ix, ref to_ix, ref text, ref result]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Substring),
            input_kinds: vec![Number, Number, Text],
            input_bindings: vec![from_ix.clone(), to_ix.clone(), text.clone()],
            output_kinds: vec![Text],
            output_bindings: vec![result.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![from_ix.clone(), to_ix.clone(), text.clone()]),
            bound_output_vars: bound_vars(&vec![result.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ has length _", [ref text, ref length]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Length),
            input_kinds: vec![Text],
            input_bindings: vec![text.clone()],
            output_kinds: vec![Number],
            output_bindings: vec![length.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![text.clone()]),
            bound_output_vars: bound_vars(&vec![length.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ is number _", [ref text, ref number]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(IsNumber),
            input_kinds: vec![Text],
            input_bindings: vec![text.clone()],
            output_kinds: vec![Number],
            output_bindings: vec![number.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![text.clone()]),
            bound_output_vars: bound_vars(&vec![number.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ is id _", [ref text, ref id]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(IsId),
            input_kinds: vec![Text],
            input_bindings: vec![text.clone()],
            output_kinds: vec![Id],
            output_bindings: vec![id.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![text.clone()]),
            bound_output_vars: bound_vars(&vec![id.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ != _", [ref a, ref b]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(NotEqual),
            input_kinds: vec![Number, Number],
            input_bindings: vec![a.clone(), b.clone()],
            output_kinds: vec![],
            output_bindings: vec![],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![a.clone(), b.clone()]),
            bound_output_vars: bound_vars(&vec![]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("count _", [ref a]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(Count),
            input_kinds: vec![],
            input_bindings: vec![],
            output_kinds: vec![Number],
            output_bindings: vec![a.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![]),
            bound_output_vars: bound_vars(&vec![a.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ = get bit _ of _", [ref bit, ref ix, ref bits]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(GetBit),
            input_kinds: vec![Number, Id],
            input_bindings: vec![ix.clone(), bits.clone()],
            output_kinds: vec![Number],
            output_bindings: vec![bit.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![ix.clone(), bits.clone()]),
            bound_output_vars: bound_vars(&vec![bit.clone()]),
            bound_aggregate_vars: bound_over_vars,
        }),
        ("_ = set bit _ of _", [ref new_bits, ref ix, ref bits]) => Some(bootstrap::Primitive{
            primitive: PrimitiveOrNegated::Primitive(SetBit),
            input_kinds: vec![Number, Id],
            input_bindings: vec![ix.clone(), bits.clone()],
            output_kinds: vec![Id],
            output_bindings: vec![new_bits.clone()],
            over_bindings: over_bindings.clone(),
            bound_input_vars: bound_vars(&vec![ix.clone(), bits.clone()]),
            bound_output_vars: bound_vars(&vec![new_bits.clone()]),
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

pub fn cache_regex<'a>(string: &String, regexes: &'a mut HashMap<String, Regex>) -> &'a Regex {
    if !regexes.contains_key(string) {
        regexes.insert(string.to_owned(), Regex::new(string).unwrap());
    }
    regexes.get(string).unwrap()
}

impl Primitive {
    pub fn apply(&self, chunk: &Chunk, input_ixes: &[usize], group_ixes: &[usize], over_ixes: &[(usize, Kind, Direction)], strings: &mut Vec<String>, regexes: &mut HashMap<String, Regex>) -> Chunk {
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
                        data.push(from_number(ordinal as f64));
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
            (Primitive::CopyString, [a]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    data.extend(row);
                    data.push(row[a]);
                    data.push(row[a+1]);
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
            // TODO making a new regex for each is wasteful when it's a constant
            (Primitive::Split, [text, regex]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    let text_string = &strings[row[text+1] as usize];
                    let regex_string = &strings[row[regex+1] as usize];
                    let mut last_from_ix = 0;
                    let mut last_to_ix = 0;
                    let mut count = 0;
                    for (result_ix, (from_ix, to_ix)) in cache_regex(regex_string, regexes).find_iter(text_string).enumerate() {
                        data.extend(row);
                        data.push(from_number(result_ix as f64));
                        data.push(from_number(last_from_ix as f64));
                        data.push(from_number(from_ix as f64));
                        data.push(from_number(last_to_ix as f64));
                        last_from_ix = from_ix;
                        last_to_ix = to_ix;
                        count += 1;
                    }
                    data.extend(row);
                    data.push(from_number(count as f64));
                    data.push(from_number(last_from_ix as f64));
                    data.push(from_number(text_string.len() as f64));
                    data.push(from_number(last_to_ix as f64));
                }
            }
            (Primitive::Search, [text, regex]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    let text_string = &strings[row[text+1] as usize];
                    let regex_string = &strings[row[regex+1] as usize];
                    for (result_ix, (from_ix, to_ix)) in cache_regex(regex_string, regexes).find_iter(text_string).enumerate() {
                        data.extend(row);
                        data.push(from_number(result_ix as f64));
                        data.push(from_number(from_ix as f64));
                        data.push(from_number(to_ix as f64));
                    }
                }
            }
            (Primitive::Capture, [text, regex]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    let text_string = &strings[row[text+1] as usize];
                    let regex_string = &strings[row[regex+1] as usize];
                    for (result_ix, capture) in cache_regex(regex_string, regexes).captures_iter(text_string).enumerate() {
                        for (capture_ix, ixes) in capture.iter_pos().enumerate() {
                            match ixes {
                                Some((from_ix, to_ix)) => {
                                    data.extend(row);
                                    data.push(from_number(capture_ix as f64));
                                    data.push(from_number(result_ix as f64));
                                    data.push(from_number(from_ix as f64));
                                    data.push(from_number(to_ix as f64));
                                }
                                None => () // optional group was not matched
                            }
                        }
                    }
                }
            }
            (Primitive::Replace, [text, regex, replacement]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    let result = {
                        let text_string = &*strings[row[text+1] as usize];
                        let regex_string = &strings[row[regex+1] as usize];
                        let replacement_string = &*strings[row[replacement+1] as usize];
                        cache_regex(regex_string, regexes).replace_all(text_string, replacement_string)
                    };
                    data.extend(row);
                    push_string(&mut data, strings, result);
                }
            }
            (Primitive::Substring, [from_ix, to_ix, text]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    let result = {
                        let text_string = &strings[row[text+1] as usize];
                        let from_ix_usize = to_number(row[from_ix]) as usize;
                        let to_ix_usize = to_number(row[to_ix]) as usize;
                        text_string[from_ix_usize..to_ix_usize].to_owned()
                    };
                    data.extend(row);
                    push_string(&mut data, strings, result);
                }
            }
            (Primitive::Length, [text]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    data.extend(row);
                    data.push(from_number(strings[row[text+1] as usize].len() as f64));
                }
            }
            (Primitive::IsNumber, [text]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    let text_string = &strings[row[text+1] as usize];
                    match text_string.parse::<f64>() {
                        Ok(number) => {
                            data.extend(row);
                            data.push(from_number(number));
                        }
                        Err(_) => ()
                    }
                }
            }
            (Primitive::IsId, [text]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    let text_string = &strings[row[text+1] as usize];
                    match text_string.parse::<u64>() {
                        Ok(id) => {
                            data.extend(row);
                            data.push(id);
                        }
                        Err(_) => ()
                    }
                }
            }
            (Primitive::NotEqual, [a,b]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    if row[a] != row[b] {
                        data.extend(row);
                    }
                }
            }
            (Primitive::Count, []) => {
                let sorted_chunk = typed_sort(chunk, over_ixes, strings).sort(group_ixes);
                for group in sorted_chunk.groups(group_ixes) {
                    let count = group.len() / chunk.row_width;
                    for row in group.chunks(chunk.row_width) {
                        data.extend(row);
                        data.push(from_number(count as f64));
                    }
                }
            }
            (Primitive::GetBit, [ix, bits]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    let bit = (row[bits] >> (to_number(row[ix]) as u64)) & 1;
                    data.extend(row);
                    data.push(from_number(bit as f64));
                }
            }
            (Primitive::SetBit, [ix, bits]) => {
                for row in chunk.data.chunks(chunk.row_width) {
                    let new_bits = row[bits] | (1 << (to_number(row[ix]) as u64));
                    data.extend(row);
                    data.push(new_bits);
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
            Primitive::CopyString => 2,
            Primitive::Min => 0,
            Primitive::Split => 4,
            Primitive::Search => 3,
            Primitive::Capture => 4,
            Primitive::Replace => 2,
            Primitive::Substring => 2,
            Primitive::Length => 1,
            Primitive::IsNumber => 1,
            Primitive::IsId => 1,
            Primitive::NotEqual => 0,
            Primitive::Count => 1,
            Primitive::GetBit => 1,
            Primitive::SetBit => 1,
        };
        Chunk{data: data, row_width: chunk.row_width + num_outputs}
    }
}