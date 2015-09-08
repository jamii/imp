use ::std::mem::size_of;
use ::std::slice::bytes::copy_memory;
use ::std::cmp::Ordering;

#[derive(Clone, Debug)]
pub struct Chunk {
    pub data: Vec<u64>,
    pub row_width: usize,
    pub sort_key: Vec<usize>,
}

pub type Id = u64;
pub type Hash = u64;
pub type Number = f64;
pub type Text = &'static String;

#[derive(Clone, Debug)]
pub enum Value {
    Id(Id),
    Number(Number),
    Text(Hash, Text),
}

#[derive(Copy, Clone, Debug)]
pub enum Kind {
    Id,
    Number,
    Text,
}

#[derive(Clone, Debug)]
pub struct Relation {
    pub fields: Vec<Id>,
    pub kinds: Vec<Kind>,
    pub chunk: Chunk,
}

#[derive(Clone, Debug)]
pub struct Groups<'a> {
    pub chunk: &'a Chunk,
    pub ix: usize,
}

#[derive(Clone, Debug)]
pub struct Diffs<'a> {
    pub left_key: &'a [usize],
    pub left_groups: Groups<'a>,
    pub left_group: Option<&'a [u64]>,
    pub right_key: &'a [usize],
    pub right_groups: Groups<'a>,
    pub right_group: Option<&'a [u64]>,
}

pub enum Diff<'a> {
    Left(&'a [u64]),
    Both(&'a [u64], &'a [u64]),
    Right(&'a [u64]),
}

pub fn compare_by_key(left_words: &[u64], left_key: &[usize], right_words: &[u64], right_key: &[usize]) -> Ordering {
    for (&left_ix, &right_ix) in left_key.iter().zip(right_key.iter()) {
        match left_words[left_ix].cmp(&right_words[right_ix]) {
            Ordering::Less => return Ordering::Less,
            Ordering::Equal => (),
            Ordering::Greater => return Ordering::Greater,
        }
    }
    return Ordering::Equal;
}

impl Chunk {
    fn sort(&mut self, key: &[usize]) {
        let row_width = self.row_width;
        let mut buffer = self.data.clone();
        for &word_ix in key.iter().rev() {
            let mut counts = [[0; 256]; 8];
            for row_ix in (0..self.data.len()).step_by(row_width) {
                let word = self.data[row_ix + word_ix];
                let bytes = unsafe{ ::std::mem::transmute::<u64, [u8; 8]>(word) };
                for byte_ix in (0..8) {
                    counts[byte_ix][bytes[byte_ix] as usize] += 1;
                }
            }
            let mut buckets = [[0; 256]; 8];
            for byte_ix in (0..8){
                for byte in (0..255) {
                    buckets[byte_ix][byte+1] = buckets[byte_ix][byte] + (row_width * counts[byte_ix][byte]);
                }
            }
            for byte_ix in (0..8) {
                for row_ix in (0..self.data.len()).step_by(row_width) {
                    let word = self.data[row_ix + word_ix];
                    let bytes = unsafe{ ::std::mem::transmute::<u64, [u8; 8]>(word) };
                    let byte = bytes[byte_ix] as usize;
                    let bucket = buckets[byte_ix][byte];
                    for ix in (0..row_width) {
                        buffer[bucket + ix] = self.data[row_ix + ix];
                    }
                    buckets[byte_ix][byte] += row_width;
                }
                ::std::mem::swap(&mut buffer, &mut self.data);
            }
        }
        self.sort_key = key.to_vec();
    }

    fn groups<'a>(&'a self) -> Groups<'a> {
        Groups{chunk: &self, ix: 0}
    }

    fn project(&self) -> Chunk {
        let mut data = vec![];
        for group in self.groups() {
            for &word_ix in self.sort_key.iter() {
                data.push(group[word_ix]);
            }
        }
        let row_width = self.sort_key.len();
        let sort_key = (0..self.sort_key.len()).collect();
        Chunk{data: data, row_width: row_width, sort_key: sort_key}
    }

    fn diffs<'a>(&'a self, other: &'a Chunk) -> Diffs<'a> {
        assert_eq!(self.sort_key.len(), other.sort_key.len());
        let mut left_groups = self.groups();
        let mut right_groups = other.groups();
        let left_group = left_groups.next();
        let right_group = right_groups.next();
        Diffs{left_key: &self.sort_key[..], left_groups: left_groups, left_group: left_group, right_key: &other.sort_key[..], right_groups: right_groups, right_group: right_group}
    }

    fn semijoin(&mut self, other: &mut Chunk) -> (Chunk, Chunk) {
        let mut self_data = Vec::with_capacity(self.data.len());
        let mut other_data = Vec::with_capacity(other.data.len());
        for diff in self.diffs(other) {
            match diff {
                Diff::Both(self_words, other_words) => {
                    self_data.extend(self_words);
                    other_data.extend(other_words);
                }
                _ => ()
            }
        }
        (
            Chunk{ data: self_data, row_width: self.row_width, sort_key: self.sort_key.clone()},
            Chunk{ data: other_data, row_width: other.row_width, sort_key: other.sort_key.clone()},
        )
    }

    fn join(&self, other: &Chunk) -> Chunk {
        let mut data = vec![];
        for diff in self.diffs(other) {
            match diff {
                Diff::Both(self_words, other_words) => {
                    for self_row in self_words.chunks(self.row_width) {
                        for other_row in other_words.chunks(self.row_width) {
                            data.extend(self_row);
                            data.extend(other_row);
                        }
                    }
                }
                _ => ()
            }
        }
        let row_width = self.row_width + other.row_width;
        let mut sort_key = self.sort_key.clone();
        for word_ix in other.sort_key.iter() {
            sort_key.push(self.row_width + word_ix);
        }
        Chunk{data: data, row_width: row_width, sort_key: sort_key}
    }

    fn union(&self, other: &Chunk) -> Chunk {
        assert_eq!(self.row_width, other.row_width);
        assert_eq!(self.sort_key, other.sort_key);
        let mut data = vec![];
        for diff in self.diffs(other) {
            match diff {
                Diff::Left(self_words) => {
                    data.extend(self_words);
                }
                Diff::Both(self_words, _) => {
                    data.extend(self_words);
                }
                Diff::Right(other_words) => {
                    data.extend(other_words);
                }
            }
        }
        Chunk{data: data, row_width: self.row_width, sort_key: self.sort_key.clone()}
    }

    fn difference(&self, other: &Chunk) -> Chunk {
        assert_eq!(self.row_width, other.row_width);
        assert_eq!(self.sort_key, other.sort_key);
        let mut data = vec![];
        for diff in self.diffs(other) {
            match diff {
                Diff::Left(self_words) => {
                    data.extend(self_words);
                }
                _ => ()
            }
        }
        Chunk{data: data, row_width: self.row_width, sort_key: self.sort_key.clone()}
    }
}

impl<'a> Iterator for Groups<'a> {
    type Item = &'a [u64];

    fn next(&mut self) -> Option<&'a [u64]> {
        let data = &self.chunk.data;
        let row_width = self.chunk.row_width;
        let key = &self.chunk.sort_key[..];
        if self.ix >= data.len() {
            None
        } else {
            let start = self.ix;
            let mut end = start;
            loop {
                end += row_width;
                if end >= data.len()
                || compare_by_key(&data[start..start+row_width], key, &data[end..end+row_width], key) != Ordering::Equal {
                    break;
                }
            }
            self.ix = end;
            Some(&data[start..end])
        }
    }
}

impl<'a> Iterator for Diffs<'a> {
    type Item = Diff<'a>;

    fn next(&mut self) -> Option<Diff<'a>> {
        match (self.left_group, self.right_group) {
            (Some(left_words), Some(right_words)) => {
                match compare_by_key(left_words, self.left_key, right_words, self.right_key) {
                    Ordering::Less => {
                        let result = Some(Diff::Left(left_words));
                        self.left_group = self.left_groups.next();
                        result
                    }
                    Ordering::Equal => {
                        let result = Some(Diff::Both(left_words, right_words));
                        self.left_group = self.left_groups.next();
                        self.right_group = self.right_groups.next();
                        result
                    }
                    Ordering::Greater => {
                        let result = Some(Diff::Right(right_words));
                        self.right_group = self.right_groups.next();
                        result
                    }
                }
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests{
    use super::*;

    use std::cmp::Ordering;
    use rand::{Rng, SeedableRng, StdRng};
    use test::{Bencher, black_box};
    use std::collections::HashSet;

    pub fn ids(seed: usize, n: usize) -> Vec<Id> {
        let mut rng = StdRng::new().unwrap();
        rng.reseed(&[seed+0, seed+1, seed+2, seed+3]);
        (0..n).map(|_| rng.gen_range(0, n as u64)).collect()
    }

    #[test]
    pub fn test_chunk_sort() {
        let ids_a = black_box(ids(7, 1000));
        let ids_b = black_box(ids(42, 1000));
        let mut ids = vec![];
        for (&id_a, &id_b) in ids_a.iter().zip(ids_b.iter()) {
            ids.push(id_a);
            ids.push(id_b);
        }
        let result_ids = {
            let mut id_pairs = ids.chunks(2).collect::<Vec<_>>();
            id_pairs.sort();
            id_pairs.into_iter().flat_map(|pair| pair).map(|&id| id).collect::<Vec<_>>()
        };
        let mut chunk = Chunk{data: ids.clone(), row_width: 2, sort_key: vec![]};
        chunk.sort(&[0, 1]);
        assert_eq!(result_ids, chunk.data);
    }

    #[test]
    pub fn test_chunk_simple_join() {
        let ids_a = black_box(ids(7, 1000));
        let ids_b = black_box(ids(42, 1000));
        let key_a = &[0];
        let key_b = &[0];
        let mut result_ids = vec![];
        for &id_a in ids_a.iter() {
            for &id_b in ids_b.iter() {
                if id_a == id_b {
                    result_ids.push(id_a);
                    result_ids.push(id_b);
                }
            }
        }
        let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 1, sort_key: vec![] };
        let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 1, sort_key: vec![] };
        chunk_a.sort(key_a);
        chunk_b.sort(key_b);
        let result_chunk = chunk_a.join(&chunk_b);
        assert_eq!(
            result_ids.chunks(2).collect::<HashSet<_>>(),
            result_chunk.data.chunks(2).collect::<HashSet<_>>()
            );
    }

    #[test]
    pub fn test_chunk_complex_join() {
        let ids_a = black_box(ids(7, 1_000));
        let ids_b = ids_a.clone(); // to ensure that at least every 1/10 ids will produce a join
        let key_a = &[0, 1];
        let key_b = &[0, 1];
        let mut result_ids = vec![];
        for row_a in ids_a.chunks(2) {
            for row_b in ids_b.chunks(5) {
                if compare_by_key(row_a, key_a, row_b, key_b) == Ordering::Equal {
                    for &id_a in row_a {
                        result_ids.push(id_a);
                    }
                    for &id_b in row_b {
                        result_ids.push(id_b);
                    }
                }
            }
        }
        let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 2, sort_key: vec![] };
        let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 5, sort_key: vec![] };
        chunk_a.sort(key_a);
        chunk_b.sort(key_b);
        let result_chunk = chunk_a.join(&chunk_b);
        assert_eq!(
            result_ids.chunks(7).collect::<HashSet<_>>(),
            result_chunk.data.chunks(7).collect::<HashSet<_>>()
            );
    }

    #[bench]
    pub fn bench_chunk_sort_1(bencher: &mut Bencher) {
        let ids = black_box(ids(7, 1_000_000));
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 1, sort_key: vec![]};
            chunk.sort(&[0]);
            black_box(&chunk);
        });
    }

    #[bench]
    pub fn bench_chunk_sort_2(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7, 1_000_000));
        let ids_b = black_box(ids(42, 1_000_000));
        let mut ids = vec![];
        for (id_a, id_b) in ids_a.into_iter().zip(ids_b.into_iter()) {
            ids.push(id_a);
            ids.push(id_b);
        }
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 2, sort_key: vec![]};
            chunk.sort(&[0, 1]);
            black_box(&chunk);
        });
    }

    #[bench]
    pub fn bench_chunk_sort_3(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7, 1_000_000));
        let ids_b = black_box(ids(42, 1_000_000));
        let ids_c = black_box(ids(75, 1_000_000));
        let mut ids = vec![];
        for ((id_a, id_b), id_c) in ids_a.into_iter().zip(ids_b.into_iter()).zip(ids_c.into_iter()) {
            ids.push(id_a);
            ids.push(id_b);
            ids.push(id_c);
        }
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 3, sort_key: vec![]};
            chunk.sort(&[0, 1, 2]);
            black_box(&chunk);
        });
    }

    #[bench]
    pub fn bench_chunk_sort_column(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7, 1_000_000));
        let ids_b = black_box(ids(42, 1_000_000));
        let ids_c = black_box(ids(75, 1_000_000));
        let mut ids = vec![];
        for ((id_a, id_b), id_c) in ids_a.into_iter().zip(ids_b.into_iter()).zip(ids_c.into_iter()) {
            ids.push(id_a);
            ids.push(id_b);
            ids.push(id_c);
        }
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 3, sort_key: vec![]};
            chunk.sort(&[0]);
            black_box(&chunk);
        });
    }

    #[bench]
    pub fn bench_chunk_simple_join(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7, 1_000_000));
        let ids_b = black_box(ids(42, 1_000_000));
        let key_a = &[0];
        let key_b = &[0];
        bencher.iter(|| {
            let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 1, sort_key: vec![] };
            let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 1, sort_key: vec![] };
            chunk_a.sort(key_a);
            chunk_b.sort(key_b);
            let result_chunk = chunk_a.join(&chunk_b);
            black_box(result_chunk);
        })
    }

    #[bench]
    pub fn bench_chunk_wide_join(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7, 1_000_000)).into_iter().flat_map(|id| vec![id, 0, 0].into_iter()).collect::<Vec<_>>();
        let ids_b = black_box(ids(42, 1_000_000)).into_iter().flat_map(|id| vec![id, 0, 0].into_iter()).collect::<Vec<_>>();
        let key_a = &[0];
        let key_b = &[0];
        bencher.iter(|| {
            let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 3, sort_key: vec![] };
            let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 3, sort_key: vec![] };
            chunk_a.sort(key_a);
            chunk_b.sort(key_b);
            let result_chunk = chunk_a.join(&chunk_b);
            black_box(result_chunk);
        })
    }
}

// impl Kind {
//     fn width(&self) -> usize {
//         let bytes = match *self {
//             Kind::Id => size_of::<Id>(),
//             Kind::Number => size_of::<Number>(),
//             Kind::Text => size_of::<(Hash, Text)>(),
//         };
//         bytes / 8
//     }

//     fn comparable_width(&self) -> usize {
//         let bytes = match *self {
//             Kind::Id => size_of::<Id>(),
//             Kind::Number => size_of::<Number>(),
//             Kind::Text => size_of::<Hash>(), // don't compare the string pointer
//         };
//         bytes / 8
//     }
// }

// impl Relation {
//     fn len(&self) -> usize {
//         self.data.len() / self.row_width
//     }

//     fn key(&self, key_fields: &[Id]) -> Vec<usize> {
//         let column_widths = self.kinds.iter().map(|kind| kind.width()).collect::<Vec<_>>();
//         let mut column_boundaries = vec![0; column_widths.len()];
//         for column in (0..column_widths.len()-1) {
//             column_boundaries[column+1] = column_boundaries[column] + column_widths[column];
//         }
//         let mut key = vec![];
//         for &key_field in key_fields {
//             let key_field_ix = self.fields.iter().position(|&field| field == key_field).unwrap();
//             let start_ix = column_boundaries[key_field_ix];
//             let end_ix = start_ix + self.kinds[key_field_ix].key_width();
//             key.extend(start_ix..end_ix)
//         }
//         key
//     }

//     fn sort(&mut self, fields: &[Id]) {
//         let key = self.key(fields);

//         self.sort_key = key;
//     }

//     fn project(&self, other_fields: &[Id]) -> Relation {
//         let other_field_ixes = other_fields.iter().map(|other_field|
//             self.fields.iter().position(|self_field| self_field == other_field).unwrap()
//             ).collect::<Vec<_>>();
//         let other_kinds = other_field_ixes.iter().map(|&other_field_ix|
//             self.kinds[other_field_ix]
//             ).collect::<Vec<_>>();
//         let other_row_width = other_kinds.iter().map(|kind| kind.width()).sum();
//         let other_len = (self.data.len() / self.row_width) * other_row_width;
//         let mut other_data = Vec::with_capacity(other_len);
//         let other_key = self.key(other_fields);
//         for row in self.data.chunks(self_row_width) {
//             for &other_key_ix in other_key.iter() {
//                 other_data.push(row[key_ix]);
//             }
//         }
//         let other = Relation{
//             fields: other_fields.to_vec(),
//             kinds: other_kinds,
//             data: other_data,
//             sort_key: vec![],
//         };
//         other
//         // TODO dedup other
//     }

//     fn total_join(&mut self, other: &mut Relation, self_key: &[usize], other_key: &[usize]) -> Relation {
//         // a join where we know that every tuple contributes to the output
//         assert_eq!(&*self.sort_key, self_key);
//         assert_eq!(&*other.sort_key, other_key);
//         assert_eq!(self_key.len(), other_key.len());
//         let self_row_width = self.kinds.iter().map(|kind| kind.width()).sum();
//         let other_row_width = other_kinds.iter().map(|kind| kind.width()).sum();
//         let result_data = vec![];
//         let self_len = self.data.len();
//         let other_len = other.data.len();
//         let mut self_ix = 0;
//         let mut other_ix = 0;
//     }
// }