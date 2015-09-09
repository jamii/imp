use ::std::mem::size_of;
use ::std::slice::bytes::copy_memory;
use ::std::cmp::{Ordering, min};

#[derive(Clone, Debug)]
pub struct Chunk {
    pub data: Vec<u64>,
    pub row_width: usize,
    pub sort_key: Vec<usize>,
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

#[derive(Clone, Debug)]
pub enum Diff<'a> {
    Left(&'a [u64]),
    Both(&'a [u64], &'a [u64]),
    Right(&'a [u64]),
}

pub fn compare_by_key(left_words: &[u64], left_key: &[usize], right_words: &[u64], right_key: &[usize]) -> Ordering {
    for ix in 0..min(left_key.len(), right_key.len()) {
        match left_words[left_key[ix]].cmp(&right_words[right_key[ix]]) {
            Ordering::Less => return Ordering::Less,
            Ordering::Equal => (),
            Ordering::Greater => return Ordering::Greater,
        }
    }
    return Ordering::Equal;
}

impl Chunk {
    pub fn empty() -> Chunk {
        Chunk{ data: vec![], row_width: 0, sort_key: vec![] }
    }

    pub fn sort(&self, key: &[usize]) -> Chunk {
        let row_width = self.row_width;
        let mut data = self.data.clone();
        let mut buffer = self.data.clone();
        for &word_ix in key.iter().rev() {
            let mut counts = [[0; 256]; 8];
            for row_ix in (0..data.len()).step_by(row_width) {
                let word = data[row_ix + word_ix];
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
                for row_ix in (0..data.len()).step_by(row_width) {
                    let word = data[row_ix + word_ix];
                    let bytes = unsafe{ ::std::mem::transmute::<u64, [u8; 8]>(word) };
                    let byte = bytes[byte_ix] as usize;
                    let bucket = buckets[byte_ix][byte];
                    for ix in (0..row_width) {
                        buffer[bucket + ix] = data[row_ix + ix];
                    }
                    buckets[byte_ix][byte] += row_width;
                }
                ::std::mem::swap(&mut buffer, &mut data);
            }
        }
        Chunk{data: data, row_width: self.row_width, sort_key: key.to_vec()}
    }

    pub fn groups<'a>(&'a self) -> Groups<'a> {
        Groups{chunk: &self, ix: 0}
    }

    pub fn project(&self) -> Chunk {
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

    pub fn diffs<'a>(&'a self, other: &'a Chunk) -> Diffs<'a> {
        assert_eq!(self.sort_key.len(), other.sort_key.len());
        let mut left_groups = self.groups();
        let mut right_groups = other.groups();
        let left_group = left_groups.next();
        let right_group = right_groups.next();
        Diffs{left_key: &self.sort_key[..], left_groups: left_groups, left_group: left_group, right_key: &other.sort_key[..], right_groups: right_groups, right_group: right_group}
    }

    pub fn semijoin(&self, other: &Chunk) -> (Chunk, Chunk) {
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

    pub fn join(&self, other: &Chunk) -> Chunk {
        let mut data = vec![];
        for diff in self.diffs(other) {
            match diff {
                Diff::Both(self_words, other_words) => {
                    for self_row in self_words.chunks(self.row_width) {
                        for other_row in other_words.chunks(other.row_width) {
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

    pub fn union(&self, other: &Chunk) -> Chunk {
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

    pub fn difference(&self, other: &Chunk) -> Chunk {
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

    pub fn ids(seed: usize, n: usize) -> Vec<u64> {
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
        chunk = chunk.sort(&[0, 1]);
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
        chunk_a = chunk_a.sort(key_a);
        chunk_b = chunk_b.sort(key_b);
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
                    result_ids.extend(row_a);
                    result_ids.extend(row_b);
                }
            }
        }
        let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 2, sort_key: vec![] };
        let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 5, sort_key: vec![] };
        chunk_a = chunk_a.sort(key_a);
        chunk_b = chunk_b.sort(key_b);
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
            chunk = chunk.sort(&[0]);
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
            chunk = chunk.sort(&[0, 1]);
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
            chunk = chunk.sort(&[0, 1, 2]);
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
            chunk = chunk.sort(&[0]);
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
            chunk_a = chunk_a.sort(key_a);
            chunk_b = chunk_b.sort(key_b);
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
            chunk_a = chunk_a.sort(key_a);
            chunk_b = chunk_b.sort(key_b);
            let result_chunk = chunk_a.join(&chunk_b);
            black_box(result_chunk);
        })
    }
}
