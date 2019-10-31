use crate::shared::*;

pub type Address = usize;

pub enum Bytecode {
    Constant(Set),
    Union(Address, Address),
    Intersect(Address, Address),
    Join(Address, Address, Vec<Vec<(usize, usize)>>),
    Equal(Address, Address),
    Negate(Address),
    Project(Address, Vec<usize>),
    Map(Address, Native, Vec<usize>),
    Reduce(Address, Address, Box<Bytecode>),
}
