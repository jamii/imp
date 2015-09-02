#![feature(test)]
#![feature(btree_b)]

extern crate rand;
extern crate test;
extern crate time;

use rand::{Rng, SeedableRng, StdRng};

macro_rules! time {
    ($name:expr, $expr:expr) => {{
        let start = ::time::precise_time_s();
        let result = $expr;
        let end = ::time::precise_time_s();
        println!("{} took {}s", $name, end - start);
        result
    }};
}

pub type Id = u32;

fn main() {
    fn ids(seed: usize) -> Vec<u32> {
        let n = 1<<27;
        let mut rng = StdRng::new().unwrap();
        rng.reseed(&[seed+0, seed+1, seed+2, seed+3]);
        (0..n)
        .map(|_| rng.gen_range(0,n))
        .collect()
    }
    let mut ids = test::black_box(ids(4));
    time!("radix sort on random", {
        radix_sort(&mut ids);
    });
    test::black_box(&ids);
    let mut ids = test::black_box((1..(1<<27)).collect());
    time!("radix sort on sorted", {
        radix_sort(&mut ids);
    });
    test::black_box(&ids);
}

fn get_byte(id: Id, offset: usize) -> usize {
    ((id >> (offset * 8)) & 0b1111_1111) as usize
}

pub fn radix_sort(ids: &mut Vec<Id>) {
    let mut buffer = ids.clone();
    for offset in (0..4) {
        {
            let ids_inner = &mut ids[..];
            let mut counts = [0; 256];
            for ix in (0..ids_inner.len()) {
                let id = unsafe{ *ids_inner.get_unchecked(ix) };
                let byte = get_byte(id, offset);
                let count = unsafe { counts.get_unchecked_mut(byte) };
                *count = *count + 1;
            }
            let mut buckets = [0; 256];
            for ix in (1..256) {
                unsafe { *buckets.get_unchecked_mut(ix) = *buckets.get_unchecked(ix-1) + *counts.get_unchecked(ix-1) };
            }
            for ix in (0..ids_inner.len()) {
                let id = unsafe{ *ids_inner.get_unchecked(ix) };
                let byte = get_byte(id, offset);
                let bucket = unsafe{ *buckets.get_unchecked(byte) };
                unsafe{ *buffer.get_unchecked_mut(bucket) = id; }
                unsafe{ *buckets.get_unchecked_mut(byte) = bucket + 1; }
            }
        }
        ::std::mem::swap(&mut buffer, ids);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashSet;
    use std::collections::BTreeSet;
    use std::cmp::{max, Ordering};

    use rand::{Rng, SeedableRng, StdRng};
    use test::{Bencher, black_box};

    fn ids(seed: usize) -> Vec<u32> {
        let n = 100_000;
        let mut rng = StdRng::new().unwrap();
        rng.reseed(&[seed+0, seed+1, seed+2, seed+3]);
        let mut seen_ids = HashSet::new();
        (0..n)
            .map(|_| rng.gen_range(0,n))
            .filter(|id| seen_ids.insert(*id)) // filter out dupes
            .collect()
    }

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
    fn bench_copy(bencher: &mut Bencher) {
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
    fn bench_radix_sort(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
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
    fn bench_small_btree_intersect(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        let max_size = max(ids_a.len(), ids_b.len());
        bencher.iter(|| {
            let mut results = Vec::with_capacity(max_size);
            let mut index = BTreeSet::with_b(10);
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
    fn bench_large_btree_intersect(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        let max_size = max(ids_a.len(), ids_b.len());
        bencher.iter(|| {
            let mut results = Vec::with_capacity(max_size);
            let mut index = BTreeSet::with_b(100);
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
    fn bench_std_sort_intersect(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        let max_size = max(ids_a.len(), ids_b.len());
        bencher.iter(|| {
            // the clone unfairly penalises this test, since in a real use we could just sort in place
            let mut sorted_a = ids_a.clone();
            let mut sorted_b = ids_b.clone();
            sorted_a.sort();
            sorted_b.sort();
            let mut results: Vec<u32> = Vec::with_capacity(max_size);
            let mut ix_a = 0;
            let mut ix_b = 0;
            loop {
                match (sorted_a.get(ix_a), sorted_b.get(ix_b)) {
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
            black_box(results);
        });
    }

    #[bench]
    fn bench_radix_sort_intersect(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        let max_size = max(ids_a.len(), ids_b.len());
        bencher.iter(|| {
            // the clone unfairly penalises this test, since in a real use we could just sort in place
            let mut sorted_a = ids_a.clone();
            let mut sorted_b = ids_b.clone();
            radix_sort(&mut sorted_a);
            radix_sort(&mut sorted_b);
            let mut results: Vec<u32> = Vec::with_capacity(max_size);
            let mut ix_a = 0;
            let mut ix_b = 0;
            loop {
                match (sorted_a.get(ix_a), sorted_b.get(ix_b)) {
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
            black_box(results);
        });
    }
}