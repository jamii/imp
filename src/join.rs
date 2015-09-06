use ::std::mem::size_of;
use ::std::slice::bytes::copy_memory;
use ::std::cmp::Ordering;

type Id = u64;
type Hash = u64;
type Number = f64;
type Text = &'static String;

#[derive(Copy, Clone, Debug)]
enum Kind {
    Id,
    Number,
    Text,
}

#[derive(Clone, Debug)]
enum Value {
    Id(Id),
    Number(Number),
    Text(Hash, Text),
}

#[derive(Clone, Debug)]
struct Chunk {
    data: Vec<u64>,
    row_width: usize,
}

#[derive(Clone, Debug)]
struct Relation {
    fields: Vec<Id>,
    kinds: Vec<Kind>,
    chunk: Chunk,
}

impl Kind {
    fn width(&self) -> usize {
        let bytes = match *self {
            Kind::Id => size_of::<Id>(),
            Kind::Number => size_of::<Number>(),
            Kind::Text => size_of::<(Hash, Text)>(),
        };
        bytes / 8
    }

    fn comparable_width(&self) -> usize {
        let bytes = match *self {
            Kind::Id => size_of::<Id>(),
            Kind::Number => size_of::<Number>(),
            Kind::Text => size_of::<Hash>(), // don't compare the string pointer
        };
        bytes / 8
    }
}

fn get_byte(word: u64, ix: usize) -> u8 {
    unsafe{ transmute::<u64, [u8; 8]>(word)[ix] }
}

fn compare_by_key(left_words: &[u64], left_key: &[usize], right_words: &[u64], right_key: &[usize]) -> Ordering {
    for (&left_ix, &right_ix) in left_key.iter().zip(right_key.iter()) {
        match left_words[left_ix].cmp(&right_words[right_ix]) {
            Ordering::Less => return Ordering::Less,
            Ordering::Equal => (),
            Ordering::Greater => return Ordering::Greater,
        }
    }
    return Ordering::Equal;
}

impl Relation {
    fn len(&self) -> usize {
        self.data.len() / self.row_width
    }

    fn key(&self, key_fields: &[Id]) -> Vec<usize> {
        let column_widths = self.kinds.iter().map(|kind| kind.width()).collect::<Vec<_>>();
        let mut column_boundaries = vec![0; column_widths.len()];
        for column in (0..column_widths.len()-1) {
            column_boundaries[column+1] = column_boundaries[column] + column_widths[column];
        }
        let mut key = vec![];
        for &key_field in key_fields {
            let key_field_ix = self.fields.iter().position(|&field| field == key_field).unwrap();
            let start_ix = column_boundaries[key_field_ix];
            let end_ix = start_ix + self.kinds[key_field_ix].key_width();
            key.extend(start_ix..end_ix)
        }
        key
    }

    fn sort(&mut self, fields: &[Id]) {
        let key = self.key(fields);
        let mut counts = (0..key.len()).map(|_| [[0; 256]; 8]).collect::<Vec<_>>();
        for row in self.data.chunks(self.row_width) {
            for (key_ix, &word_ix) in key.iter().enumerate() {
                let word = row[word_ix]
                for byte_ix in (0..8) {
                    let byte = get_byte(word, byte_ix) as usize;
                    counts[key_ix][byte_ix][byte] += 1;
                }
            }
        }
        let mut buckets = (0..key.len()*8).map(|_| [[0; 256]; 8]).collect::<Vec<_>>();
        for key_ix in 0..key.len() {
            for byte_ix in 0..8 {
                for byte in (0..255) {
                    buckets[key_ix][byte_ix][byte+1] = buckets[key_ix][byte_ix][byte] + counts[key_ix][byte_ix][byte];
                }
            }
        }
        let mut buffer = self.data.clone();
        for (key_ix, &word_ix) in key.iter().enumerate().rev() {
            for byte_ix in (0..8) {
                for row in self.data.chunks(self.row_width) {
                    let bucket_ix = (key_ix * 8) + byte_ix;
                    let byte = get_byte(row[word_ix], byte_ix) as usize;
                    let bucket = buckets[bucket_ix][byte];
                    buffer[bucket] = row;
                    buckets[bucket_ix][byte] = bucket + 1;
                }
                ::std::mem::swap(&mut buffer, &mut self.data);
            }
        }
        self.sort_key = key;
    }

    fn project(&self, other_fields: &[Id]) -> Relation {
        let other_field_ixes = other_fields.iter().map(|other_field|
            self.fields.iter().position(|self_field| self_field == other_field).unwrap()
            ).collect::<Vec<_>>();
        let other_kinds = other_field_ixes.iter().map(|&other_field_ix|
            self.kinds[other_field_ix]
            ).collect::<Vec<_>>();
        let other_row_width = other_kinds.iter().map(|kind| kind.width()).sum();
        let other_len = (self.data.len() / self.row_width) * other_row_width;
        let mut other_data = Vec::with_capacity(other_len);
        let other_key = self.key(other_fields);
        for row in self.data.chunks(self_row_width) {
            for &other_key_ix in other_key.iter() {
                other_data.push(row[key_ix]);
            }
        }
        let other = Relation{
            fields: other_fields.to_vec(),
            kinds: other_kinds,
            data: other_data,
            sort_key: vec![],
        };
        other
        // TODO dedup other
    }

    fn total_join(&mut self, other: &mut Relation, self_key: &[usize], other_key: &[usize]) -> Relation {
        // a join where we know that every tuple contributes to the output
        assert_eq!(&*self.sort_key, self_key);
        assert_eq!(&*other.sort_key, other_key);
        assert_eq!(self_key.len(), other_key.len());
        let self_row_width = self.kinds.iter().map(|kind| kind.width()).sum();
        let other_row_width = other_kinds.iter().map(|kind| kind.width()).sum();
        let result_data = vec![];
        let self_len = self.data.len();
        let other_len = other.data.len();
        let mut self_ix = 0;
        let mut other_ix = 0;
    }
}