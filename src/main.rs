#![feature(test)]

extern crate rand;
extern crate test;
extern crate time;

use rand::{Rng, SeedableRng, StdRng};
use std::cmp::{max, Ordering};

macro_rules! time {
    ($name:expr, $expr:expr) => {{
        let start = ::time::precise_time_s();
        let result = $expr;
        let end = ::time::precise_time_s();
        println!("{} took {}s", $name, end - start);
        result
    }};
}

pub fn ids(seed: usize) -> Vec<Id> {
    let mut rng = StdRng::new().unwrap();
    rng.reseed(&[seed+0, seed+1, seed+2, seed+3]);
    (0..100_000).map(|_| rng.gen_range(0, 100_000)).collect()
}

pub type Id = u64;

fn main() {
    let mut ids = test::black_box(ids(4));
    time!("radix sort on random", {
        radix_sort(&mut ids);
    });
    test::black_box(&ids);
    let mut ids = test::black_box((1..(1<<29)).collect());
    time!("radix sort on sorted", {
        radix_sort(&mut ids);
    });
    test::black_box(&ids);
}

pub fn radix_sort(ids: &mut Vec<Id>) {
    let ids: &mut Vec<[u8; 8]> = unsafe{ ::std::mem::transmute(ids) };
    let mut buffer = ids.clone();
    for offset in (0..8) {
        let mut counts = [0; 256];
        for id in ids.iter() {
            let byte = id[offset] as usize;
            counts[byte] += 1;
        }
        let mut buckets = [0; 256];
        for ix in (1..256) {
            buckets[ix] = buckets[ix-1] + counts[ix-1];
        }
        for id in ids.iter() {
            let byte = id[offset] as usize;
            buffer[buckets[byte]] = *id;
            buckets[byte] += 1;
        }
        ::std::mem::swap(&mut buffer, ids);
    }
}

pub fn intersect_sorted(ids_a: &Vec<Id>, ids_b: &Vec<Id>) -> Vec<Id> {
    let mut results = Vec::with_capacity(max(ids_a.len(), ids_b.len()));
    let mut ix_a = 0;
    let mut ix_b = 0;
    loop {
        match (ids_a.get(ix_a), ids_b.get(ix_b)) {
            (Some(&a), Some(&b)) => {
                match a.cmp(&b) {
                    Ordering::Less => {
                        ix_a += 1;
                    }
                    Ordering::Equal => {
                        // a real join would have to have a nested loop here to deal with dupes
                        results.push(a);
                        ix_a += 1;
                        ix_b += 1;
                    }
                    Ordering::Greater => {
                        ix_b += 1;
                    }
                }
            }
            _ => break,
        }
    }
    results
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashSet;
    use std::collections::BTreeSet;
    use std::cmp::max;

    use test::{Bencher, black_box};

    #[test]
    fn test_radix_sort() {
        let mut ids_a = ids(7);
        let mut ids_b = ids(7);
        ids_a.sort();
        radix_sort(&mut ids_b);
        assert_eq!(ids_a, ids_b);
    }

    #[bench]
    fn bench_sum(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
        bencher.iter(|| {
            let mut sum = 0;
            for ix in (0..ids.len()) {
                unsafe{ sum += *ids.get_unchecked(ix); }
            }
            black_box(sum);
        });
    }

    #[bench]
    fn bench_copy_sequential(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
        let mut buffer = ids.clone();
        bencher.iter(|| {
            for ix in (0..ids.len()) {
                unsafe{ *buffer.get_unchecked_mut(ix) = *ids.get_unchecked(ix); }
            }
            black_box(&buffer);
        });
    }

    #[bench]
    fn bench_copy_small_random(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
        let mut buffer = ids.clone();
        bencher.iter(|| {
            for ix in (0..ids.len()) {
                let id = unsafe{ *ids.get_unchecked(ix) };
                unsafe{ *buffer.get_unchecked_mut(id as usize % 256) = id; }
            }
            black_box(&buffer);
        });
    }

    #[bench]
    fn bench_copy_random(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
        let mut buffer = ids.clone();
        bencher.iter(|| {
            for ix in (0..ids.len()) {
                let id = unsafe{ *ids.get_unchecked(ix) };
                unsafe{ *buffer.get_unchecked_mut(id as usize) = id; }
            }
            black_box(&buffer);
        });
    }

    #[bench]
    fn bench_radix_sort(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
        bencher.iter(|| {
            let mut ids = ids.clone();
            radix_sort(&mut ids);
            black_box(&ids);
        });
    }

    #[bench]
    fn bench_radix_sort_presorted(bencher: &mut Bencher) {
        let ids = black_box((1..100_000).collect::<Vec<_>>());
        bencher.iter(|| {
            let mut ids = ids.clone();
            radix_sort(&mut ids);
            black_box(&ids);
        });
    }

    #[bench]
    fn bench_hash_intersect(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        let max_size = max(ids_a.len(), ids_b.len());
        bencher.iter(|| {
            let mut results = Vec::with_capacity(max_size);
            let mut index = HashSet::with_capacity(max_size);
            for id in ids_a.iter() {
                index.insert(*id);
            }
            for id in ids_b.iter() {
                if index.contains(id) {
                    results.push(*id);
                }
            }
            black_box(results);
        });
    }

    #[bench]
    fn bench_hash_index_intersect(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        let max_size = max(ids_a.len(), ids_b.len());
        let mut index = HashSet::with_capacity(max_size);
        for id in ids_a.iter() {
            index.insert(*id);
        }
        bencher.iter(|| {
            let mut results = Vec::with_capacity(max_size);
            for id in ids_b.iter() {
                if index.contains(id) {
                    results.push(*id);
                }
            }
            black_box(results);
        });
    }

    #[bench]
    fn bench_std_sort_intersect(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        bencher.iter(|| {
            // the clone unfairly penalises this test, since in a real use we could just sort in place
            let mut sorted_a = ids_a.clone();
            let mut sorted_b = ids_b.clone();
            sorted_a.sort();
            sorted_b.sort();
            let results = intersect_sorted(&sorted_a, &sorted_b);
            black_box(results);
        });
    }

    #[bench]
    fn bench_sorted_index_intersect(bencher: &mut Bencher) {
        let mut ids_a = black_box(ids(7));
        let mut ids_b = black_box(ids(42));
        ids_a.sort();
        ids_b.sort();
        bencher.iter(|| {
            let results = intersect_sorted(&ids_a, &ids_b);
            black_box(results);
        });
    }

    #[bench]
    fn bench_radix_sort_intersect(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        bencher.iter(|| {
            // the clone unfairly penalises this test, since in a real use we could just sort in place
            let mut sorted_a = ids_a.clone();
            let mut sorted_b = ids_b.clone();
            radix_sort(&mut sorted_a);
            radix_sort(&mut sorted_b);
            let results = intersect_sorted(&sorted_a, &sorted_b);
            black_box(results);
        });
    }
}