use std::cmp::{Ordering, min};
use std::hash;
use std::rc::Rc;
use std::mem::size_of;
use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Chunk {
    pub data: Vec<u64>,
    pub row_width: usize,
}

#[derive(Clone, Debug)]
pub struct Groups<'a> {
    pub chunk: &'a Chunk,
    pub key: &'a [usize],
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

pub fn compare_by_key(left_words: &[u64], right_words: &[u64], left_key: &[usize], right_key: &[usize]) -> Ordering {
    for ix in 0..min(left_key.len(), right_key.len()) {
        match left_words[left_key[ix]].cmp(&right_words[right_key[ix]]) {
            Ordering::Less => return Ordering::Less,
            Ordering::Equal => (),
            Ordering::Greater => return Ordering::Greater,
        }
    }
    return Ordering::Equal;
}

const PIECE_SIZE: usize = 8;
const NUM_PIECES: usize = 8;
const MAX_PIECE: usize = 256;

pub fn get_piece(word: u64, ix: usize) -> usize {
    ((word >> ((ix * PIECE_SIZE) as u64)) & 0b1111_1111) as usize
}

impl Chunk {
    pub fn empty() -> Chunk {
        Chunk{ data: vec![], row_width: 1 }
    }

    pub fn len(&self) -> usize {
        self.data.len() / self.row_width
    }

    pub fn sort(&self, key: &[usize]) -> Chunk {
        let row_width = self.row_width;
        let mut data = self.data.clone();
        let mut buffer = self.data.clone();
        for &word_ix in key.iter().rev() {
            let mut counts = [[0; MAX_PIECE]; NUM_PIECES];
            for row_ix in (0..data.len()).step_by(row_width) {
                let word = data[row_ix + word_ix];
                for piece_ix in (0..NUM_PIECES) {
                    counts[piece_ix][get_piece(word,piece_ix)] += 1;
                }
            }
            let mut buckets = [[0; MAX_PIECE]; NUM_PIECES];
            for piece_ix in (0..NUM_PIECES) {
                for piece in (0..255) {
                    buckets[piece_ix][piece+1] = buckets[piece_ix][piece] + (row_width * counts[piece_ix][piece]);
                }
            }
            for piece_ix in (0..NUM_PIECES) {
                for row_ix in (0..data.len()).step_by(row_width) {
                    let word = data[row_ix + word_ix];
                    let piece = get_piece(word, piece_ix);
                    let bucket = buckets[piece_ix][piece];
                    for ix in (0..row_width) {
                        buffer[bucket + ix] = data[row_ix + ix];
                    }
                    buckets[piece_ix][piece] += row_width;
                }
                ::std::mem::swap(&mut buffer, &mut data);
            }
        }
        Chunk{data: data, row_width: self.row_width}
    }

    pub fn groups<'a>(&'a self, key: &'a [usize]) -> Groups<'a> {
        Groups{chunk: &self, key: key, ix: 0}
    }

    pub fn project(&self, key: &[usize]) -> Chunk {
        // project
        let mut data = vec![];
        for row in self.data.chunks(self.row_width) {
            for &word_ix in key.iter() {
                data.push(row[word_ix]);
            }
        }
        let mut result = Chunk{data: data, row_width: key.len()};

        // sort
        let key = (0..key.len()).collect::<Vec<_>>();
        result = result.sort(&key[..]);

        // dedup
        let mut data = vec![];
        for group in result.groups(&key[..]) {
            data.extend(&group[..key.len()]);
        }
        result.data = data;
        result
    }

    pub fn diffs<'a>(&'a self, other: &'a Chunk, self_key: &'a [usize], other_key: &'a [usize]) -> Diffs<'a> {
        let mut left_groups = self.groups(self_key);
        let mut right_groups = other.groups(other_key);
        let left_group = left_groups.next();
        let right_group = right_groups.next();
        Diffs{left_key: self_key, left_groups: left_groups, left_group: left_group, right_key: other_key, right_groups: right_groups, right_group: right_group}
    }

    pub fn semijoin(&self, other: &Chunk, self_key: &[usize], other_key: &[usize]) -> (Chunk, Chunk) {
        let mut self_data = Vec::with_capacity(self.data.len());
        let mut other_data = Vec::with_capacity(other.data.len());
        for diff in self.diffs(other, self_key, other_key) {
            match diff {
                Diff::Both(self_words, other_words) => {
                    self_data.extend(self_words);
                    other_data.extend(other_words);
                }
                _ => ()
            }
        }
        (
            Chunk{ data: self_data, row_width: self.row_width},
            Chunk{ data: other_data, row_width: other.row_width},
        )
    }

    pub fn join(&self, other: &Chunk, self_key: &[usize], other_key: &[usize]) -> Chunk {
        let mut data = vec![];
        for diff in self.diffs(other, self_key, other_key) {
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
        Chunk{data: data, row_width: row_width}
    }

    pub fn union(&self, other: &Chunk, key: &[usize]) -> Chunk {
        let mut data = vec![];
        for diff in self.diffs(other, &key[..], &key[..]) {
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
        Chunk{data: data, row_width: self.row_width}
    }

    pub fn difference(&self, other: &Chunk, key: &[usize]) -> Chunk {
        let mut data = vec![];
        for diff in self.diffs(other, &key[..], &key[..]) {
            match diff {
                Diff::Left(self_words) => {
                    data.extend(self_words);
                }
                _ => ()
            }
        }
        Chunk{data: data, row_width: self.row_width}
    }

    pub fn selfjoin(&self, left_ix: usize, right_ix: usize) -> Chunk {
        let mut data = vec![];
        for row in self.data.chunks(self.row_width) {
            if row[left_ix] == row[right_ix] {
                data.extend(row);
            }
        }
        Chunk{ data: data, row_width: self.row_width}
    }

    pub fn filter(&self, ix: usize, value: u64) -> Chunk {
        let mut data = vec![];
        for row in self.data.chunks(self.row_width) {
            if row[ix] == value {
                data.extend(row);
            }
        }
        Chunk{ data: data, row_width: self.row_width}
    }
}

impl<'a> Iterator for Groups<'a> {
    type Item = &'a [u64];

    fn next(&mut self) -> Option<&'a [u64]> {
        let data = &self.chunk.data;
        let row_width = self.chunk.row_width;
        if self.ix >= data.len() {
            None
        } else {
            let start = self.ix;
            let mut end = start;
            loop {
                end += row_width;
                if end >= data.len()
                || compare_by_key(&data[start..start+row_width], &data[end..end+row_width], self.key, self.key) != Ordering::Equal {
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
                match compare_by_key(left_words, right_words, self.left_key, self.right_key) {
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
            (Some(left_words), None) => {
                let result = Some(Diff::Left(left_words));
                self.left_group = self.left_groups.next();
                result
            }
            (None, Some(right_words)) => {
                let result = Some(Diff::Right(right_words));
                self.right_group = self.right_groups.next();
                result
            }
            (None, None) => None,
        }
    }
}

pub type Id = u64;
pub type Hash = u64;
pub type Number = f64;
pub type Text = &'static String;

// these just make type signatures easier to read
pub type ColumnId = usize;
pub type FieldId = Id;
pub type ClauseId = Id;
pub type ViewId = Id;
pub type VariableId = Id;

#[derive(Copy, PartialEq, Eq, Clone, Debug)]
pub enum Kind {
    Id,
    Number,
    Text,
}

impl Kind {
    pub fn width(&self) -> usize {
        let bytes = match *self {
            Kind::Id => size_of::<Id>(),
            Kind::Number => size_of::<Number>(),
            Kind::Text => size_of::<(Hash, Text)>(),
        };
        bytes / 8
    }
}

pub fn hash<T: hash::Hash>(t: &T) -> u64 {
    let mut s = hash::SipHasher::new();
    hash::Hash::hash(t, &mut s);
    hash::Hasher::finish(&s)
}

#[derive(Clone, Debug)]
pub enum Action {
    Sort(usize, Vec<usize>),
    Project(usize, Vec<usize>),
    SemiJoin(usize, usize, Vec<usize>, Vec<usize>),
    Join(usize, usize, Vec<usize>, Vec<usize>),
    SelfJoin(usize, usize, usize),
    Filter(usize, usize, u64),
    DebugChunk(usize),
    DebugText(usize, usize),
}

#[derive(Clone, Debug)]
pub struct Query {
    pub upstream: Vec<usize>,
    pub actions: Vec<Action>,
    pub result: usize,
}

impl Query {
    pub fn run(&self, strings: &Vec<String>, states: &[Rc<Chunk>]) -> Chunk {
        // TODO use COW chunks
        let mut chunks = self.upstream.iter().map(|&ix| Cow::Borrowed(&*states[ix])).collect::<Vec<_>>();
        for action in self.actions.iter() {
            // println!("");
            // println!("{:?}", chunks.iter().map(|chunk| chunk.len()).collect::<Vec<_>>());
            // println!("{:?}", action);
            // time!(format!("{:?}", action), {
            match action {
                &Action::Sort(ix, ref key) => {
                    let chunk = chunks[ix].sort(&key[..]);
                    chunks[ix] = Cow::Owned(chunk);
                },
                &Action::Project(ix, ref key) => {
                    let chunk = chunks[ix].project(&key[..]);
                    chunks[ix] = Cow::Owned(chunk);
                }
                &Action::SemiJoin(left_ix, right_ix, ref left_key, ref right_key) => {
                    let (left_chunk, right_chunk) = chunks[left_ix].semijoin(&chunks[right_ix], &left_key[..], &right_key[..]);
                    chunks[left_ix] = Cow::Owned(left_chunk);
                    chunks[right_ix] = Cow::Owned(right_chunk);
                },
                &Action::Join(left_ix, right_ix, ref left_key, ref right_key) => {
                    let chunk = chunks[left_ix].join(&chunks[right_ix], &left_key[..], &right_key[..]);
                    chunks[left_ix] = Cow::Owned(Chunk::empty());
                    chunks[right_ix] = Cow::Owned(chunk);
                }
                &Action::SelfJoin(ix, left_column, right_column) => {
                    let chunk = chunks[ix].selfjoin(left_column, right_column);
                    chunks[ix] = Cow::Owned(chunk);
                }
                &Action::Filter(ix, column, value) => {
                    let chunk = chunks[ix].filter(column, value);
                    chunks[ix] = Cow::Owned(chunk);
                }
                &Action::DebugChunk(ix) => {
                    let chunk = &chunks[ix];
                    println!("{:?}", chunk.data.chunks(chunk.row_width).collect::<Vec<_>>());
                }
                &Action::DebugText(ix, field) => {
                    let chunk = &chunks[ix];
                    println!("{:?}", chunk.data.chunks(chunk.row_width).map(|row| &strings[row[field] as usize]).collect::<Vec<_>>());
                }
            }
            // });
        }
        ::std::mem::replace(&mut chunks[self.result], Cow::Owned(Chunk::empty())).into_owned()
    }
}

#[derive(Debug, Clone)]
pub struct Union {
    pub upstream: Vec<usize>,
    pub members: Vec<Member>,
    pub key: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Member {
    Insert,
    Remove,
}

impl Union {
    fn run(&self, states: &[Rc<Chunk>]) -> Chunk {
        assert_eq!(self.members[0], Member::Insert);
        let mut result = Cow::Borrowed(&*states[self.upstream[0]]);
        for ix in 1..self.upstream.len() {
            result = Cow::Owned(match self.members[ix] {
                Member::Insert => result.union(&*states[self.upstream[ix]], &self.key[..]),
                Member::Remove => result.difference(&*states[self.upstream[ix]], &self.key[..]),
            });
        }
        result.into_owned()
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    pub ids: Vec<ViewId>,
    // TODO store field ids too
    pub schemas: Vec<Vec<Kind>>,
    pub states: Vec<Rc<Chunk>>,
    pub views: Vec<View>,
    pub downstreams: Vec<Vec<usize>>,
    pub dirty: Vec<bool>, // should be BitSet but that has been removed from std :(

    pub strings: Vec<String>, // to be replaced by gc
}

#[derive(Clone, Debug)]
pub enum View {
    Input,
    Query(Query),
    Union(Union),
}

impl Program {
    pub fn get_state(&self, id: ViewId) -> &Chunk {
        let &Program{ref ids, ref states, ..} = self;
        let ix = ids.iter().position(|&other_id| other_id == id).unwrap();
        &states[ix]
    }

    // TODO require Relation?
    pub fn set_state(&mut self, id: ViewId, state: Chunk) {
        let &mut Program{ref ids, ref mut states, ref views, ref downstreams, ref mut dirty, ..} = self;
        let ix = ids.iter().position(|&other_id| other_id == id).unwrap();
        match views[ix] {
            View::Input => {
                states[ix] = Rc::new(state);
                for &downstream_ix in downstreams[ix].iter() {
                    dirty[downstream_ix] = true;
                }
            }
            _ => {
                panic!("Can't set view {:?} - it isn't an input!", id);
            }
        }
    }

    pub fn run(&mut self) {
        let &mut Program{ref mut states, ref views, ref downstreams, ref mut dirty, ref strings, ..} = self;
        while let Some(ix) = dirty.iter().position(|&is_dirty| is_dirty) {
            dirty[ix] = false;
            let new_chunk = match views[ix] {
                View::Input => panic!("How did an input get dirtied?"),
                View::Query(ref query) => query.run(strings, &states[..]),
                View::Union(ref union) => union.run(&states[..])
            };
            // TODO using != assumes both will have the same sort order. is that safe?
            if *states[ix] != new_chunk {
                states[ix] = Rc::new(new_chunk);
                for &downstream_ix in downstreams[ix].iter() {
                    dirty[downstream_ix] = true;
                }
            }
        }
    }
}

#[cfg(test)]
pub mod tests{
    use super::*;

    use std::cmp::Ordering;
    use rand::{Rng, SeedableRng, StdRng};
    use test::{Bencher, black_box};
    use std::collections::HashSet;
    use std::io::prelude::*;
    use std::fs::File;
    use std::rc::Rc;

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
        let mut chunk = Chunk{data: ids.clone(), row_width: 2};
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
        let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 1 };
        let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 1 };
        chunk_a = chunk_a.sort(key_a);
        chunk_b = chunk_b.sort(key_b);
        let result_chunk = chunk_a.join(&chunk_b, key_a, key_b);
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
                if compare_by_key(row_a, row_b, key_a, key_b) == Ordering::Equal {
                    result_ids.extend(row_a);
                    result_ids.extend(row_b);
                }
            }
        }
        let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 2 };
        let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 5 };
        chunk_a = chunk_a.sort(key_a);
        chunk_b = chunk_b.sort(key_b);
        let result_chunk = chunk_a.join(&chunk_b, key_a, key_b);
        assert_eq!(
            result_ids.chunks(7).collect::<HashSet<_>>(),
            result_chunk.data.chunks(7).collect::<HashSet<_>>()
            );
    }

    #[bench]
    pub fn bench_chunk_sort_1(bencher: &mut Bencher) {
        let ids = black_box(ids(7, 1_000_000));
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 1};
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
            let mut chunk = Chunk{data: ids.clone(), row_width: 2};
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
            let mut chunk = Chunk{data: ids.clone(), row_width: 3};
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
            let mut chunk = Chunk{data: ids.clone(), row_width: 3};
            chunk = chunk.sort(&[0]);
            black_box(&chunk);
        });
    }

    #[bench]
    pub fn bench_chunk_sort_range(bencher: &mut Bencher) {
        let ids = black_box((1..10_000).collect::<Vec<_>>());
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 1};
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
            let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 1 };
            let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 1 };
            chunk_a = chunk_a.sort(key_a);
            chunk_b = chunk_b.sort(key_b);
            let result_chunk = chunk_a.join(&chunk_b, key_a, key_b);
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
            let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 3 };
            let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 3 };
            chunk_a = chunk_a.sort(key_a);
            chunk_b = chunk_b.sort(key_b);
            let result_chunk = chunk_a.join(&chunk_b, key_a, key_b);
            black_box(result_chunk);
        })
    }

    fn from_tsv(filename: &'static str, kinds: Vec<Kind>, strings: &mut Vec<String>) -> Chunk {
        let mut tsv = String::new();
        File::open(filename).unwrap().read_to_string(&mut tsv).unwrap();
        let mut lines = tsv.lines();
        lines.next(); // drop header
        let mut data = vec![];
        for line in lines {
            for (kind, field) in kinds.iter().zip(line.split("\t")) {
                match *kind {
                    Kind::Id => data.push(field.parse::<u64>().unwrap()),
                    Kind::Number => data.push(field.parse::<f64>().unwrap() as u64),
                    Kind::Text => {
                        let field = field.to_owned();
                        data.push(hash(&field));
                        data.push(strings.len() as u64);
                        strings.push(field);
                    }
                }
            }
        }
        let row_width = kinds.iter().map(|kind| kind.width()).sum();
        Chunk{data: data, row_width: row_width}
    }

    pub fn chinook() -> (Vec<String>, Vec<Rc<Chunk>>) {
        use super::Kind::*;
        let mut strings = vec![];
        let states = vec![
            Rc::new(from_tsv("data/Artist.csv", vec![Id, Text], &mut strings)),
            Rc::new(from_tsv("data/Album.csv", vec![Id, Text, Id], &mut strings)),
            Rc::new(from_tsv("data/Track.csv", vec![Id, Text, Id, Id, Id, Text, Number, Number, Number], &mut strings)),
            Rc::new(from_tsv("data/PlaylistTrack.csv", vec![Id, Id], &mut strings)),
            Rc::new(from_tsv("data/Playlist.csv", vec![Id, Text], &mut strings)),
        ];
        (strings, states)
    }

    #[test]
    fn test_chinook() {
        chinook();
    }

    fn chinook_metal(mut strings: Vec<String>, mut states: Vec<Rc<Chunk>>) -> Chunk {
        use super::Action::*;
        let metal = "Heavy Metal Classic".to_owned();
        let query_state = Chunk{ data: vec![hash(&metal), strings.len() as u64], row_width: 2};
        strings.push(metal);
        states.push(Rc::new(query_state));
        let query = Query{
            upstream: vec![0,1,2,3,4,5],
            actions: vec![
            // semijoin Query and Playlist on Name
            Sort(5, vec![0]),
            Sort(4, vec![1]),
            SemiJoin(5, 4, vec![0], vec![1]),

            // semijoin Playlist and PlaylistTrack on PlaylistId
            Sort(4, vec![0]),
            Sort(3, vec![0]),
            SemiJoin(4, 3, vec![0], vec![0]),

            // semijoin PlaylistTrack and Track on TrackId
            Sort(3, vec![1]),
            Project(2, vec![0, 3]),
            SemiJoin(3, 2, vec![1], vec![0]),

            // semijoin Track and Album on AlbumId
            Sort(2, vec![1]),
            Project(1, vec![0, 3]),
            SemiJoin(2, 1, vec![1], vec![0]),

            // join Artist and Album on ArtistId
            Sort(0, vec![0]),
            Sort(1, vec![1]),
            Join(0, 1, vec![0], vec![1]),

            // join AlbumId/Name and Track on AlbumId
            Project(1, vec![3, 1, 2]),
            Join(1, 2, vec![0], vec![1]),

            // join TrackId/Name and PlaylistTrack on TrackId
            Project(2, vec![3, 1, 2]),
            Join(2, 3, vec![0], vec![1]),

            // join PlaylistId/Name and Playlist on PlaylistId
            Project(3, vec![3, 1, 2]),
            Join(3, 4, vec![0], vec![0]),

            // join Name/Name and Query on Name
            Project(4, vec![4, 5, 1, 2]),
            Join(4, 5, vec![0], vec![0]),

            // project Name without hash
            Project(5, vec![3]),
            ],

            result: 5
        };
        query.run(&strings, &states[..])
    }

    #[test]
    fn test_chinook_metal() {
        let (strings, states) = chinook();
        let results = chinook_metal(strings.clone(), states);
        assert_eq!(
            results.data.iter().map(|ix| &strings[*ix as usize]).collect::<Vec<_>>(),
            vec!["AC/DC", "Accept", "Black Sabbath", "Metallica", "Iron Maiden", "Mot\u{f6}rhead", "M\u{f6}tley Cr\u{fc}e", "Ozzy Osbourne", "Scorpions"]
            );
    }

    #[bench]
    pub fn bench_chinook_metal(bencher: &mut Bencher) {
        let (strings, states) = chinook();
        bencher.iter(|| {
            black_box(chinook_metal(strings.clone(), states.clone()));
        });
    }
}