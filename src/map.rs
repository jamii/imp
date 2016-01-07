#[derive(Clone, Debug)]
pub struct Node<T> {
    leaf_bitmap: u32,
    node_bitmap: u32,
    leaves: Vec<T>,
    nodes: Vec<Node<T>>,
}

#[derive(Clone, Debug)]
pub struct Tree<T> {
    root: Node<T>
}

unsafe fn any_lifetime<'a, T>(x: &T) -> &'a T {
    ::std::mem::transmute(x)
}

unsafe fn any_lifetime_mut<'a, T>(x: &mut T) -> &'a mut T {
    ::std::mem::transmute(x)
}

const KEY_LENGTH: usize = 13;

fn chunk_at(key: u64, ix: usize) -> u32 {
    ((key >> (ix*5)) & 0b11111) as u32
}

fn count_ones_after(bitmap: u32, chunk: u32) -> usize {
    (((bitmap as u64) << (32 - chunk)) as u32).count_ones() as usize
}

impl Node<u64> {
    fn singleton(row: &[u64], mut column: usize, mut ix: usize) -> Self {
        if ix >= KEY_LENGTH {
            column += 1;
            ix = 0;
            if column >= row.len() {
                panic!("Out of bits!");
            }
        }
        let value = row[column];
        let key = value;
        let chunk = chunk_at(key, ix);
        Node{
            leaf_bitmap: 1 << chunk,
            node_bitmap: 0,
            leaves: row.to_vec(),
            nodes: vec![],
        }
    }
}

impl Tree<u64> {
    pub fn new() -> Self {
        Tree{
            root: Node{
                leaf_bitmap: 0,
                node_bitmap: 0,
                leaves: vec![],
                nodes: vec![],
            }
        }
    }

    pub fn insert(&mut self, row: &[u64]) {
        let mut node = &mut self.root;
        for column in 0..row.len() {
            let value = row[column];
            let key = value;
            for ix in 0..KEY_LENGTH {
                let chunk = chunk_at(key, ix);
                let mask = 1 << chunk;
                if (node.node_bitmap & mask) > 0 {
                    let node_ix = count_ones_after(node.node_bitmap, chunk);
                    node = unsafe{ any_lifetime_mut(&mut node.nodes[node_ix]) };
                    // continue loop
                } else if (node.leaf_bitmap & mask) > 0 {
                    let leaf_ix = row.len() * count_ones_after(node.leaf_bitmap, chunk);
                    if row == &node.leaves[leaf_ix..(leaf_ix + row.len())] {
                        return; // was a dupe
                    } else {
                        let leaf = node.leaves[leaf_ix..(leaf_ix + row.len())].to_vec();
                        node.leaves.drain(leaf_ix..(leaf_ix + row.len()));
                        let child = Node::singleton(&leaf[..], column, ix+1);
                        node.leaf_bitmap ^= mask;
                        node.node_bitmap |= mask;
                        let node_ix = count_ones_after(node.node_bitmap, chunk);
                        node.nodes.insert(node_ix, child);
                        node = unsafe{ any_lifetime_mut(&mut node.nodes[node_ix]) };
                        // continue loop
                    }
                } else {
                    node.leaf_bitmap |= mask;
                    let leaf_ix = row.len() * count_ones_after(node.leaf_bitmap, chunk);
                    // gross...
                    for i in 0..row.len() {
                        node.leaves.insert(leaf_ix + i, row[i]);
                    }
                    return; // inserted
                }
            }
        }
        panic!("Out of bits!");
    }

    pub fn contains(&self, row: &[u64]) -> bool {
        let mut node = &self.root;
        for column in 0..row.len() {
            let value = row[column];
            let key = value;
            for ix in 0..KEY_LENGTH {
                let chunk = chunk_at(key, ix);
                let mask = 1 << chunk as u32;
                if (node.node_bitmap & mask) > 0 {
                    let node_ix = count_ones_after(node.node_bitmap, chunk);
                    node = unsafe{ any_lifetime(&node.nodes[node_ix]) };
                    // continue loop
                } else if (node.leaf_bitmap & mask) > 0 {
                    let leaf_ix = row.len() * count_ones_after(node.leaf_bitmap, chunk);
                    let leaf = &node.leaves[leaf_ix..(leaf_ix + row.len())];
                    return row == leaf;
                } else {
                    return false;
                }
            }
        }
        panic!("Out of bits!");
    }
}

pub fn ids(seed: usize, n: usize) -> Vec<u64> {
    use rand::{Rng, SeedableRng, StdRng};
    let mut rng = StdRng::new().unwrap();
    rng.reseed(&[seed+0, seed+1, seed+2, seed+3]);
    (0..n).map(|_| rng.gen()).collect()
}

pub fn main() {
    let ids = ids(7, 1000000);
    let mut tree: Tree<u64> = Tree::new();
    for ix in 0..ids.len() {
        tree.insert(&ids[ix..ix+1]);
    }
    println!("{:?}", (0..ids.len()).filter(|&ix| !tree.contains(&ids[ix..ix+1])).count());
    print!("{:?}", tree.contains(&[1000000]));
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use test::{Bencher, black_box};

    #[bench]
    pub fn bench_map_build_1(bencher: &mut Bencher) {
        let ids = black_box(ids(7, 1000000));
        bencher.iter(|| {
            let mut tree: Tree<u64> = Tree::new();
            for ix in 0..ids.len() {
                tree.insert(&ids[ix..ix+1]);
            }
            black_box(&tree);
        });
    }

    #[bench]
    pub fn bench_map_baseline_1(bencher: &mut Bencher) {
        let ids = black_box(ids(7, 1000000));
        bencher.iter(|| {
            let mut ids = ids.clone();
            ids.sort();
            black_box(&ids);
        });
    }
}
