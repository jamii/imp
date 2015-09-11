use std::mem::size_of;

use chunk::*;

pub type Id = u64;
pub type Hash = u64;
pub type Number = f64;
pub type Text = &'static String;

// these just make type signatures easier to read
pub type ColumnId = usize;
pub type FieldId = Id;
pub type ClauseId = Id;
pub type ViewId = Id;
pub type VariableId = Id;

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
    pub fields: Vec<FieldId>,
    pub kinds: Vec<Kind>,
    pub chunk: Chunk,
}

impl Kind {
    pub fn width(&self) -> usize {
        let bytes = match *self {
            Kind::Id => size_of::<Id>(),
            Kind::Number => size_of::<Number>(),
            Kind::Text => size_of::<(Hash, Text)>(),
        };
        bytes / 8
    }
}
