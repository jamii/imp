use rand::{Rng, SeedableRng, StdRng};
use std::cmp::{max, Ordering};

pub fn main() {
    let mut ids_a = ids(7);
    let mut ids_b = ids(42);
    let results = radix_intersect(&mut ids_a, &mut ids_b);
    println!("{:?}", results);
}

pub fn ids(seed: usize) -> Vec<Id> {
    let mut rng = StdRng::new().unwrap();
    rng.reseed(&[seed+0, seed+1, seed+2, seed+3]);
    let n = 1_000_000;
    (0..n).map(|_| rng.gen_range(0, n)).collect()
}

pub type Id = u64;

pub fn radix_sort(ids: &mut Vec<Id>) {
    let ids: &mut Vec<[u8; 8]> = unsafe{ ::std::mem::transmute(ids) };
    let mut buffer = ids.clone();
    let mut counts = [[0; 256]; 8];
    for id in ids.iter() {
        for offset in (0..8) {
            counts[offset][id[offset] as usize] += 1
        }
    }
    let mut buckets = [[0; 256]; 8];
    for offset in (0..8) {
        for ix in (1..256) {
            buckets[offset][ix] = buckets[offset][ix-1] + counts[offset][ix-1];
        }
    }
    for offset in (0..8) {
        for id in ids.iter() {
            let byte = id[offset] as usize;
            buffer[buckets[offset][byte]] = *id;
            buckets[offset][byte] += 1;
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
                        let mut end_ix_a = ix_a;
                        while ids_a.get(end_ix_a) == Some(&a) { end_ix_a += 1; }
                        let mut end_ix_b = ix_b;
                        while ids_b.get(end_ix_b) == Some(&b) { end_ix_b += 1; }
                        for ix in (ix_a..end_ix_a) {
                            for _ in (ix_b..end_ix_b) {
                                results.push(ids_a[ix]);
                            }
                        }
                        ix_a = end_ix_a;
                        ix_b = end_ix_b;
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

pub fn radix_partition(ids: &mut [Id], key: usize) -> [usize; 257] {
    let ids: &mut [[u8; 8]] = unsafe{ ::std::mem::transmute(ids) };
    let mut counts = [0; 256];
    for id in ids.iter() {
        counts[id[key] as usize] += 1;
    }
    let mut starts = [0; 257];
    let mut ends = [0; 257];
    for ix in (0..256) {
        starts[ix+1] = starts[ix] + counts[ix];
        ends[ix+1] = ends[ix] + counts[ix];
    }
    for byte in (0..256) {
        for ix in ends[byte]..starts[byte+1] {
            loop {
                let id_byte = ids[ix][key] as usize;
                if id_byte != byte {
                    ids.swap(ix, ends[id_byte]);
                    ends[id_byte] += 1;
                } else {
                    break;
                }
            }
        }
    }
    starts
}

pub fn radix_intersect_inner(ids_a: &mut [Id], ids_b: &mut [Id], key: usize, results: &mut Vec<Id>) {
    if (ids_a.len() * ids_b.len() < 16) || (key >= 8) { // arbitrary constant
        for id_a in ids_a.iter() {
            for id_b in ids_b.iter() {
                if id_a == id_b {
                    results.push(*id_a);
                }
            }
        }
    } else {
        let starts_a = radix_partition(ids_a, key);
        let starts_b = radix_partition(ids_b, key);
        for byte in (0..256) {
            radix_intersect_inner(&mut ids_a[starts_a[byte]..starts_a[byte+1]], &mut ids_b[starts_b[byte]..starts_b[byte+1]], key+1, results);
        }
    }
}

pub fn radix_intersect(mut ids_a: &mut [Id], mut ids_b: &mut [Id]) -> Vec<Id> {
    let mut results = vec![];
    radix_intersect_inner(&mut ids_a[..], &mut ids_b[..], 0, &mut results);
    results
}

pub fn loop_intersect(ids_a: &Vec<Id>, ids_b: &Vec<Id>) -> Vec<Id> {
    let mut results = vec![];
    for id_a in ids_a.iter() {
        for id_b in ids_b.iter() {
            if id_a == id_b {
                results.push(*id_a);
            }
        }
    }
    results
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashSet;
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

    #[test]
    fn test_radix_sort_intersect() {
        let mut ids_a = ids(7);
        let mut ids_b = ids(42);
        radix_sort(&mut ids_a);
        radix_sort(&mut ids_b);
        let results = intersect_sorted(&ids_a, &ids_b);
        assert_eq!(
            results.iter().collect::<HashSet<_>>(),
            loop_intersect(&ids_a, &ids_b).iter().collect::<HashSet<_>>()
            );
    }

    #[test]
    fn test_radix_intersect() {
        let ids_a = ids(7);
        let ids_b = ids(42);
        let results = radix_intersect(&mut ids_a.clone(), &mut ids_b.clone());
        assert_eq!(
            results.iter().collect::<HashSet<_>>(),
            loop_intersect(&ids_a, &ids_b).iter().collect::<HashSet<_>>()
            );
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
    fn bench_std_sort(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
        bencher.iter(|| {
            let mut ids = ids.clone();
            ids.sort();
            black_box(&ids);
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
    fn bench_radix_count(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
        bencher.iter(|| {
            let ids: &Vec<[u8; 8]> = unsafe{ ::std::mem::transmute(&ids) };
            let mut counts = [[0; 256]; 8];
            for id in ids.iter() {
                for offset in (0..8) {
                    counts[offset][id[offset] as usize] += 1
                }
            }
            let mut buckets = [[0; 256]; 8];
            for offset in (0..8) {
                for ix in (1..256) {
                    buckets[offset][ix] = buckets[offset][ix-1] + counts[offset][ix-1];
                }
            }
            black_box(&ids);
        });
    }

    #[bench]
    fn bench_radix_partition(bencher: &mut Bencher) {
        let ids = black_box(ids(7));
        bencher.iter(|| {
            let mut ids = ids.clone();
            black_box(radix_partition(&mut ids, 0));
        });
    }

    #[bench]
    fn bench_radix_sort_presorted(bencher: &mut Bencher) {
        let mut ids = black_box(ids(7));
        radix_sort(&mut ids);
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

    #[bench]
    fn bench_radix_intersect(bencher: &mut Bencher) {
        let ids_a = black_box(ids(7));
        let ids_b = black_box(ids(42));
        bencher.iter(|| {
            // the clone unfairly penalises this test, since in a real use we could just sort in place
            let results = radix_intersect(&mut ids_a.clone(), &mut ids_b.clone());
            black_box(results);
        });
    }
}