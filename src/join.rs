use ::std::mem::size_of;
use ::std::slice::bytes::copy_memory;
use ::std::cmp::Ordering;

#[derive(Clone, Debug)]
pub struct Chunk {
    pub data: Vec<u64>,
    pub row_width: usize,
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

fn get_byte(word: u64, ix: usize) -> u8 {
    unsafe{ ::std::mem::transmute::<u64, [u8; 8]>(word)[ix] }
}

impl Chunk {
    fn sort(&mut self, key: &[usize]) {
        let row_width = self.row_width;
        let mut buffer = self.data.clone();
        for &word_ix in key.iter() {
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
                    let byte = get_byte(self.data[row_ix + word_ix], byte_ix) as usize;
                    let bucket = buckets[byte_ix][byte];
                    for ix in (0..row_width) {
                        buffer[bucket + ix] = self.data[row_ix + ix];
                    }
                    buckets[byte_ix][byte] += row_width;
                }
                ::std::mem::swap(&mut buffer, &mut self.data);
            }
        }
    }
}

#[cfg(test)]
mod tests{
    use super::*;

    use rand::{Rng, SeedableRng, StdRng};
    use test::{Bencher, black_box};

    pub fn ids(seed: usize) -> Vec<Id> {
        let mut rng = StdRng::new().unwrap();
        rng.reseed(&[seed+0, seed+1, seed+2, seed+3]);
        let n = 1_000_000;
        (0..n).map(|_| rng.gen_range(0, n)).collect()
    }

    #[bench]
    pub fn bench_chunk_sort_1(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 1};
            chunk.sort(&[0]);
            black_box(&chunk);
        });
    }

    #[bench]
    pub fn bench_chunk_sort_2(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        let mut ids = vec![];
        for (id_a, id_b) in ids_a.into_iter().zip(ids_b.into_iter()) {
            ids.push(id_a);
            ids.push(id_b);
        }
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 2};
            chunk.sort(&[0, 1]);
            black_box(&chunk);
        });
    }

    #[bench]
    pub fn bench_chunk_sort_3(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        let ids_c = black_box(ids(75));
        let mut ids = vec![];
        for ((id_a, id_b), id_c) in ids_a.into_iter().zip(ids_b.into_iter()).zip(ids_c.into_iter()) {
            ids.push(id_a);
            ids.push(id_b);
            ids.push(id_c);
        }
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 3};
            chunk.sort(&[0, 1, 2]);
            black_box(&chunk);
        });
    }

    #[bench]
    pub fn bench_chunk_sort_column(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        let ids_c = black_box(ids(75));
        let mut ids = vec![];
        for ((id_a, id_b), id_c) in ids_a.into_iter().zip(ids_b.into_iter()).zip(ids_c.into_iter()) {
            ids.push(id_a);
            ids.push(id_b);
            ids.push(id_c);
        }
        bencher.iter(|| {
            let mut chunk = Chunk{data: ids.clone(), row_width: 3};
            chunk.sort(&[0]);
            black_box(&chunk);
        });
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

// fn compare_by_key(left_words: &[u64], left_key: &[usize], right_words: &[u64], right_key: &[usize]) -> Ordering {
//     for (&left_ix, &right_ix) in left_key.iter().zip(right_key.iter()) {
//         match left_words[left_ix].cmp(&right_words[right_ix]) {
//             Ordering::Less => return Ordering::Less,
//             Ordering::Equal => (),
//             Ordering::Greater => return Ordering::Greater,
//         }
//     }
//     return Ordering::Equal;
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