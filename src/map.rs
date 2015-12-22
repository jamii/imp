extern crate alloc;

use std::ptr;
use std::mem;
use self::alloc::heap;
use std::slice;
use std::fmt::{Debug, Formatter, self};

#[derive(Copy, Clone)]
struct Header {
    node_bitmap: u32,
    leaf_bitmap: u32,
}

type Leaf = [u64];

struct Node {
    start: *mut u8,
}

#[derive(Debug)]
struct Layout {
    leaf_bytes: usize,
    number_of_nodes: usize,
    number_of_leaves: usize,
    total_node_bytes: usize,
    total_leaf_bytes: usize,
    used_bytes: usize,
    allocated_bytes: usize,
}

pub struct Tree {
    leaf_bytes: usize,
    root: Node,
}

impl Node {
    fn new() -> Self {
        // everything needs to be 64 bit aligned
        assert_eq!(mem::size_of::<Header>() % 8, 0);
        assert_eq!(mem::size_of::<Node>() % 8, 0);
        unsafe{
            let start = heap::allocate(mem::size_of::<Header>().next_power_of_two(), mem::align_of::<u64>());
            *(start as *mut Header) = Header{node_bitmap: 0, leaf_bitmap: 0};
            Node{start: start}
        }
    }

    fn get_header(&self) -> Header {
        unsafe {
            *(self.start as *mut Header)
        }
    }

    fn layout(&self, leaf_bytes: usize) -> Layout {
        let number_of_nodes = self.get_header().node_bitmap.count_ones() as usize;
        let number_of_leaves = self.get_header().leaf_bitmap.count_ones() as usize;
        let total_node_bytes = number_of_nodes * mem::size_of::<Node>();
        let total_leaf_bytes = number_of_leaves * leaf_bytes;
        let used_bytes = mem::size_of::<Header>() + total_node_bytes + total_leaf_bytes;
        let allocated_bytes = used_bytes.next_power_of_two();
        Layout{
            leaf_bytes: leaf_bytes,
            number_of_nodes: number_of_nodes,
            number_of_leaves: number_of_leaves,
            total_node_bytes: total_node_bytes,
            total_leaf_bytes: total_leaf_bytes,
            used_bytes: used_bytes,
            allocated_bytes: allocated_bytes,
        }
    }

    fn get_node_offset(&self, pos: usize) -> usize {
        let ix = self.get_header().node_bitmap.wrapping_shl((32 - pos) as u32).count_ones() as usize;
        mem::size_of::<Header>() + (mem::size_of::<Node>() * ix)
    }

    fn get_leaf_offset(&self, pos: usize, layout: &mut Layout) -> usize {
        let ix = self.get_header().leaf_bitmap.wrapping_shl((32 - pos) as u32).count_ones() as usize;
        layout.allocated_bytes - ((ix + 1) * layout.leaf_bytes)
    }

    fn insert_node(&mut self, mut layout: &mut Layout, node: Node, pos: usize) {
        while layout.used_bytes + mem::size_of::<Node>() > layout.allocated_bytes {
            self.increase_size(layout);
        }
        unsafe {
            let node_offset = self.get_node_offset(pos);
            let later_node_bytes = (mem::size_of::<Header>() + layout.total_node_bytes) - node_offset;
            ptr::copy(self.start.offset(node_offset as isize), self.start.offset((node_offset + 1) as isize), later_node_bytes);
            *(self.start.offset(node_offset as isize) as *mut Node) = node;
            (*(self.start as *mut Header)).node_bitmap |= 1 << pos;
        }
    }

    fn insert_leaf(&mut self, mut layout: &mut Layout, leaf: &[u64], pos: usize) {
        while layout.used_bytes + layout.leaf_bytes > layout.allocated_bytes {
            self.increase_size(layout);
        }
        unsafe {
            let leaf_offset = self.get_leaf_offset(pos, layout);
            let leaves_offset = layout.allocated_bytes - layout.total_leaf_bytes;
            ptr::copy(
                self.start.offset(leaves_offset as isize),
                self.start.offset((leaves_offset - layout.leaf_bytes) as isize),
                leaf_offset + layout.leaf_bytes - leaves_offset
                );
            ptr::copy(leaf.as_ptr() as *const u8, self.start.offset(leaf_offset as isize), layout.leaf_bytes);
            (*(self.start as *mut Header)).leaf_bitmap |= 1 << pos;
        }
    }

    fn increase_size(&mut self, layout: &mut Layout) {
        unsafe {
            self.start = heap::reallocate(
                self.start,
                layout.allocated_bytes,
                layout.allocated_bytes * 2,
                mem::align_of::<u64>()
                );
            let old_leaves = self.start.offset((layout.allocated_bytes - layout.total_leaf_bytes) as isize);
            layout.allocated_bytes *= 2;
            let new_leaves = self.start.offset((layout.allocated_bytes - layout.total_leaf_bytes) as isize);
            ptr::copy(old_leaves, new_leaves, layout.total_leaf_bytes);
        }
    }

    fn as_slice<'a>(&'a self, leaf_bytes: usize) -> &'a [u64] {
        let layout = self.layout(leaf_bytes);
        unsafe {
            slice::from_raw_parts(self.start as *const u64, layout.allocated_bytes / 8)
        }
    }

    fn leaves(&self, leaf_bytes: usize, leaves: &mut Vec<&[u64]>) {
        let layout = self.layout(leaf_bytes);
        for leaf_ix in 0..layout.number_of_leaves {
            let leaf_offset = layout.allocated_bytes - ((leaf_ix + 1) * leaf_bytes);
            leaves.push(unsafe{
                slice::from_raw_parts(self.start.offset(leaf_offset as isize) as *const u64, leaf_bytes / 8)
            });
        }
        for node_ix in 0..layout.number_of_nodes {
            let node_offset = mem::size_of::<Header>() + (node_ix * mem::size_of::<Node>());
            let node = unsafe{
                &*(self.start.offset(node_offset as isize) as *const Node)
            };
            node.leaves(leaf_bytes, leaves);
        }
    }
}

struct DebugNode<'a> {
    leaf_bytes: usize,
    node: &'a Node,
}

impl<'a> Debug for DebugNode<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        let layout = self.node.layout(self.leaf_bytes);
        let mut ds = formatter.debug_struct("Node");
        ds.field("leaf_bitmap", &format!("{:0<32b}", self.node.get_header().leaf_bitmap));
        ds.field("node_bitmap", &format!("{:0<32b}", self.node.get_header().node_bitmap));
        ds.field("leaves", &(0..layout.number_of_leaves).map(|leaf_ix| {
            let leaf_offset = layout.allocated_bytes - ((leaf_ix + 1) * self.leaf_bytes);
            unsafe {
                slice::from_raw_parts(self.node.start.offset(leaf_offset as isize) as *const u64, self.leaf_bytes / 8)
            }
            }).collect::<Vec<_>>());
        ds.field("nodes", &(0..layout.number_of_nodes).map(|node_ix| {
            let node_offset = mem::size_of::<Header>() + (node_ix * mem::size_of::<Node>());
            let node = unsafe{
                &*(self.node.start.offset(node_offset as isize) as *const Node)
            };
            DebugNode{leaf_bytes: self.leaf_bytes, node: node}
            }).collect::<Vec<_>>());
        ds.finish()
    }
}

impl Debug for Tree {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        let mut ds = formatter.debug_struct("Tree");
        ds.field("root", &DebugNode{leaf_bytes: self.leaf_bytes, node: &self.root});
        ds.finish()
    }
}

impl Tree {
    fn new(leaf_bytes: usize) -> Self {
        Tree{
            leaf_bytes: leaf_bytes,
            root: Node::new(),
        }
    }

    fn insert (&mut self, leaf: &[u64]) {
        assert_eq!(self.leaf_bytes, leaf.len() * 8);
        let mut node = &mut self.root;
        for word_ix in 0..leaf.len() {
            let word = leaf[word_ix];
            for ix in 0..13 {
                println!("looking at {:?}", node.as_slice(self.leaf_bytes));
                let pos = (word >> (ix*5)) & 0b11111;
                let header = node.get_header();
                let mut layout = node.layout(self.leaf_bytes);
                if (header.node_bitmap & (1 << (pos as u32))) > 0 {
                    println!("descending");
                    let child_offset = node.get_node_offset(pos as usize);
                    let child = unsafe {
                        &mut *(node.start.offset(child_offset as isize) as *mut Node)
                    };
                    node = child;
                } else if (header.leaf_bitmap & (1 << (pos as u32))) > 0 {
                    println!("blocked");
                    let mut layout = node.layout(self.leaf_bytes);
                    let blocking_leaf_offset = node.get_leaf_offset(pos as usize, &mut layout);
                    let blocking_leaf = unsafe {
                        slice::from_raw_parts(node.start.offset(blocking_leaf_offset as isize) as *mut u64, self.leaf_bytes / 8)
                    };
                    println!("{:?}", (leaf, blocking_leaf));
                    if leaf == blocking_leaf {
                        println!("dupe");
                        return;
                    } else {
                        println!("collision");
                        // TODO delete leaf
                        let mut child = Node::new();
                        let (next_word_ix, next_ix) = if ix == 12 {
                            assert!(word_ix + 1 < leaf.len());
                            (word_ix + 1, 0)
                        } else {
                            (word_ix, ix + 1)
                        };
                        let next_pos = (leaf[next_word_ix] >> (next_ix * 5)) & 0b11111;
                        let mut child_layout = child.layout(self.leaf_bytes);
                        child.insert_leaf(&mut child_layout, leaf, next_pos as usize);
                        let next_blocking_pos = (blocking_leaf[next_word_ix] >> (next_ix * 5)) & 0b11111;
                        let mut child_layout = child.layout(self.leaf_bytes);
                        child.insert_leaf(&mut child_layout, blocking_leaf, next_blocking_pos as usize);
                        node.insert_node(&mut layout, child, pos as usize);
                    }
                } else {
                    println!("inserting");
                    node.insert_leaf(&mut layout, leaf, pos as usize);
                    return;
                }
            }
        }
    }

    fn leaves<'a>(&'a self) -> Vec<&'a [u64]> {
        let mut leaves = vec![];
        self.root.leaves(self.leaf_bytes, &mut leaves);
        leaves
    }
}

pub fn ids(seed: usize, n: usize) -> Vec<u64> {
    use rand::{Rng, SeedableRng, StdRng};
    let mut rng = StdRng::new().unwrap();
    rng.reseed(&[seed+0, seed+1, seed+2, seed+3]);
    (0..n).map(|_| rng.gen_range(0, n as u64)).collect()
}

pub fn main() {
    let ids = ids(7, 1_000);
    let mut tree = Tree::new(8);
    for id in ids.iter() {
        println!("{:#?}", tree);
        println!("inserting {}", id);
        tree.insert(&[*id]);
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    use rand::{Rng, SeedableRng, StdRng};
    use test::{Bencher, black_box};

    #[bench]
    pub fn bench_map_build_1(bencher: &mut Bencher) {
        let ids = black_box(ids(7, 1_000));
        bencher.iter(|| {
            let mut tree = Tree::new(8);
            for id in ids.iter() {
                tree.insert(&[*id]);
            }
            black_box(&tree);
        });
    }
}