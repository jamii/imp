

pub type Id = u64;
pub type Hash = u64;
pub type Number = f64;
pub type Text = &'static String;

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
    pub fields: Vec<Id>,
    pub kinds: Vec<Kind>,
    pub chunk: Chunk,
}