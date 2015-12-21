extern crate alloc;

use std::ptr;
use std::mem;
use self::alloc::heap;
use std::slice;

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

    fn get_node_offset(&self, pos: usize) -> isize {
        let ix = (self.get_header().node_bitmap << (32 - pos)).count_ones() as isize;
        (mem::size_of::<Header>() as isize) + ((mem::size_of::<Node> as isize) * ix)
    }

    fn get_leaf_offset(&self, pos: usize, layout: &mut Layout) -> isize {
        let ix = (self.get_header().leaf_bitmap << (32 - pos)).count_ones() as isize;
        (layout.allocated_bytes as isize) - ((ix + 1) * (layout.leaf_bytes as isize))
    }

    fn insert_node(&mut self, mut layout: &mut Layout, node: Node, pos: usize) {
        while layout.used_bytes + mem::size_of::<Node>() > layout.allocated_bytes {
            self.increase_size(layout);
        }
        unsafe {
            let node_offset = self.get_node_offset(pos);
            let later_node_bytes = mem::size_of::<Header>() + layout.total_node_bytes - (node_offset as usize);
            ptr::copy(self.start.offset(node_offset), self.start.offset(node_offset + 1), later_node_bytes);
            *(self.start.offset(node_offset) as *mut Node) = node;
        }
    }

    fn insert_leaf(&mut self, mut layout: &mut Layout, leaf: &[u64], pos: usize) {
        while layout.used_bytes + layout.leaf_bytes > layout.allocated_bytes {
            self.increase_size(layout);
        }
        unsafe {
            println!("layout claims {:?}", &layout);
            (*(self.start as *mut Header)).leaf_bitmap |= 1 << pos;
            let leaf_offset = self.get_leaf_offset(pos, layout);
            let leaves_offset = (layout.allocated_bytes as isize) - (layout.total_leaf_bytes as isize);
            println!("copying {:?} {:?} {:?}", leaves_offset, leaves_offset - (layout.leaf_bytes as isize), (leaf_offset - leaves_offset + (layout.leaf_bytes as isize)) as usize);
            ptr::copy(
                self.start.offset(leaves_offset),
                self.start.offset(leaves_offset - (layout.leaf_bytes as isize)),
                (leaf_offset - leaves_offset + (layout.leaf_bytes as isize)) as usize
                );
            println!("copying {:?} {:?} {:?}", "leaf", leaf_offset, layout.leaf_bytes);
            ptr::copy(leaf.as_ptr() as *const u8, self.start.offset(leaf_offset), layout.leaf_bytes);
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
            println!("moving {:?} {:?} {:?}",
                (layout.allocated_bytes - layout.total_leaf_bytes) as isize,
                ((layout.allocated_bytes * 2) - layout.total_leaf_bytes) as isize,
                layout.total_leaf_bytes);
            let old_leaves = self.start.offset((layout.allocated_bytes - layout.total_leaf_bytes) as isize);
            layout.allocated_bytes *= 2;
            let new_leaves = self.start.offset((layout.allocated_bytes - layout.total_leaf_bytes) as isize);
            ptr::copy(old_leaves, new_leaves, layout.total_leaf_bytes);
        }
    }

    fn as_slice<'a>(&'a self, leaf_bytes: usize) -> &'a mut [u64] {
        let layout = self.layout(leaf_bytes);
        unsafe {
            slice::from_raw_parts_mut(self.start as *mut u64, layout.allocated_bytes / 8)
        }
    }
}

pub fn main() {
    let leaf_bytes = 3 * 8;
    let mut node = Node::new();
    println!("{:?}", node.layout(leaf_bytes));
    println!("{:?}", node.as_slice(leaf_bytes));
    let mut layout = node.layout(leaf_bytes);
    node.insert_leaf(&mut layout, &[1, 2, 3], 3);
    println!("{:?}", node.layout(leaf_bytes));
    println!("{:?}", node.as_slice(leaf_bytes));
    let mut layout = node.layout(leaf_bytes);
    node.insert_leaf(&mut layout, &[4, 5, 6], 4);
    println!("{:?}", node.layout(leaf_bytes));
    println!("{:?}", node.as_slice(leaf_bytes));
    let mut layout = node.layout(leaf_bytes);
    node.insert_leaf(&mut layout, &[7, 8, 9], 2);
    println!("{:?}", node.layout(leaf_bytes));
    println!("{:?}", node.as_slice(leaf_bytes));
    println!("after");
}