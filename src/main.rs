#![feature(test)]
#![feature(collections)]
#![feature(collections_drain)]

extern crate rand;
extern crate test;
extern crate time;

macro_rules! time {
    ($name:expr, $expr:expr) => {{
        let start = ::time::precise_time_s();
        let result = $expr;
        let end = ::time::precise_time_s();
        println!("{} took {}s", $name, end - start);
        result
    }};
}

fn main() {
    let mut nums = test::black_box((0..(1 << 27)).collect::<Vec<_>>());
    let mut buffers = make_buffers(nums.len() / 256);
    time!("radix sort", {
        radix_sort(&mut nums, &mut buffers);
    });
    test::black_box(&nums);
    println!("{:?} {:?}", nums[0], nums[nums.len()-1]);
}

fn get_byte(num: usize, offset: usize) -> usize {
    (num >> offset) & 0b1111_1111
}

pub fn make_buffers(len: usize) -> Vec<Vec<usize>> {
    (0..256).map(|_| Vec::with_capacity(len)).collect::<Vec<_>>()
}

pub fn radix_sort(nums: &mut Vec<usize>, buffers: &mut Vec<Vec<usize>>) {
    for i in (0..8) {
        let offset = i * 8;
        for ix in (0..nums.len()) {
            let num = unsafe{ *nums.get_unchecked(ix) };
            let byte = get_byte(num, offset);
            let buffer = unsafe{ buffers.get_unchecked_mut(byte) };
            buffer.push(num);
        }
        unsafe{ nums.set_len(0); }
        for buffer in buffers.iter_mut() {
            for ix in (0..buffer.len()) {
                let num = unsafe{ *buffer.get_unchecked(ix) };
                nums.push(num);
            }
            unsafe{ buffer.set_len(0); }
        }
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

    fn nums(seed: usize) -> Vec<usize> {
        let n = 100_000;
        let mut rng = StdRng::new().unwrap();
        rng.reseed(&[seed+0, seed+1, seed+2, seed+3]);
        let mut seen_nums = HashSet::new();
        (0..n)
            .map(|_| rng.gen_range(0,n))
            .filter(|num| seen_nums.insert(*num)) // filter out dupes
            .collect()
    }

    #[test]
    fn test_radix_sort() {
        let mut nums_a = nums(7);
        let mut nums_b = nums(7);
        let mut nums_c = nums(7);
        nums_a.sort();
        let mut buffers = make_buffers(nums_b.len());
        radix_sort(&mut nums_b, &mut buffers);
        radix_sort(&mut nums_c, &mut buffers);
        assert_eq!(nums_a, nums_b);
        assert_eq!(nums_a, nums_c);
    }

    #[bench]
    fn bench_radix_sort(bencher: &mut Bencher) {
        let nums = black_box(nums(7));
        let mut buffers = make_buffers(nums.len());
        bencher.iter(|| {
            let mut nums = nums.clone();
            radix_sort(&mut nums, &mut buffers);
            nums
        });
    }

    #[bench]
    fn bench_hash_intersect(bencher: &mut Bencher) {
        let nums_a = black_box(nums(7));
        let nums_b = black_box(nums(42));
        let max_size = max(nums_a.len(), nums_b.len());
        bencher.iter(|| {
            let mut results = Vec::with_capacity(max_size);
            let mut index = HashSet::with_capacity(max_size);
            for num in nums_a.iter() {
                index.insert(*num);
            }
            for num in nums_b.iter() {
                if index.contains(num) {
                    results.push(*num);
                }
            }
            return results
        });
    }

    #[bench]
    fn bench_small_btree_intersect(bencher: &mut Bencher) {
        let nums_a = black_box(nums(7));
        let nums_b = black_box(nums(42));
        let max_size = max(nums_a.len(), nums_b.len());
        bencher.iter(|| {
            let mut results = Vec::with_capacity(max_size);
            let mut index = BTreeSet::with_b(10);
            for num in nums_a.iter() {
                index.insert(*num);
            }
            for num in nums_b.iter() {
                if index.contains(num) {
                    results.push(*num);
                }
            }
            return results
        });
    }

    #[bench]
    fn bench_large_btree_intersect(bencher: &mut Bencher) {
        let nums_a = black_box(nums(7));
        let nums_b = black_box(nums(42));
        let max_size = max(nums_a.len(), nums_b.len());
        bencher.iter(|| {
            let mut results = Vec::with_capacity(max_size);
            let mut index = BTreeSet::with_b(100);
            for num in nums_a.iter() {
                index.insert(*num);
            }
            for num in nums_b.iter() {
                if index.contains(num) {
                    results.push(*num);
                }
            }
            return results
        });
    }

    #[bench]
    fn bench_std_sort_intersect(bencher: &mut Bencher) {
        let nums_a = black_box(nums(7));
        let nums_b = black_box(nums(42));
        let max_size = max(nums_a.len(), nums_b.len());
        bencher.iter(|| {
            // the clone unfairly penalises this test, since in a real use we could just sort in place
            let mut sorted_a = nums_a.clone();
            let mut sorted_b = nums_b.clone();
            sorted_a.sort();
            sorted_b.sort();
            let mut results: Vec<usize> = Vec::with_capacity(max_size);
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
            return results
        });
    }

    #[bench]
    fn bench_radix_sort_intersect(bencher: &mut Bencher) {
        let nums_a = black_box(nums(7));
        let nums_b = black_box(nums(42));
        let max_size = max(nums_a.len(), nums_b.len());
        let mut buffers = make_buffers(max_size);
        bencher.iter(|| {
            // the clone unfairly penalises this test, since in a real use we could just sort in place
            let mut sorted_a = nums_a.clone();
            let mut sorted_b = nums_b.clone();
            radix_sort(&mut sorted_a, &mut buffers);
            radix_sort(&mut sorted_b, &mut buffers);
            let mut results: Vec<usize> = Vec::with_capacity(max_size);
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
            return results
        });
    }
}