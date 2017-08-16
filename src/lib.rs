use std::fmt::{Debug, Formatter, Error};
use std::collections::{HashSet, HashMap};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Kind {
    name: String,
    attributes: Vec<Attribute>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Entity {
    kind: Kind,
    values: Vec<Value>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Attribute {
    name: String,
    kind: Kind,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Entity(Entity),
    Attribute(Attribute),
    String(String),
}

impl From<String> for Value {
    fn from(string: String) -> Value {
        Value::String(string)
    }
}

impl<'a> From<&'a str> for Value {
    fn from(str: &str) -> Value {
        Value::String(str.into())
    }
}

pub struct Data {
    eav: Vec<(Entity, Attribute, Value)>,
}

impl Data {
    pub fn new() -> Self {
        Data { eav: vec![] }
    }

    pub fn add_eav(&mut self, entity: Entity, attribute: Attribute, value: Value) {
        self.eav.push((entity, attribute, value));
    }

    pub fn add_e(&mut self, entity: Entity) {
        for (attribute, value) in
            entity.kind.attributes.clone().into_iter().zip(
                entity
                    .values
                    .clone(),
            )
        {
            assert_eq!(attribute.kind, entity.kind);
            self.add_eav(entity.clone(), attribute, value);
        }
    }
}

impl Debug for Data {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let mut extra_attributes: HashMap<&Kind, HashSet<&Attribute>> = HashMap::new();
        let mut rows: HashMap<&Kind, HashMap<&Entity, HashMap<&Attribute, &Value>>> =
            HashMap::new();

        for &(ref e, ref a, ref v) in &self.eav {
            if !a.kind.attributes.contains(a) {
                extra_attributes
                    .entry(&a.kind)
                    .or_insert_with(|| HashSet::new())
                    .insert(a);
            }
            rows.entry(&e.kind)
                .or_insert_with(|| HashMap::new())
                .entry(e)
                .or_insert_with(|| HashMap::new())
                .insert(a, v);
        }

        f.write_str("Data {\n")?;
        for (kind, kind_extra_attributes) in extra_attributes {
            f.write_str(&kind.name)?;
            f.write_str("\n")?;
            for _ in 0..kind.name.len() {
                f.write_str("-")?;
            }
            f.write_str("\n")?;
            let attributes: Vec<&Attribute> = kind.attributes
                .iter()
                .chain(kind_extra_attributes.into_iter())
                .collect();
            for a in &attributes {
                f.write_str(&a.name)?;
                f.write_str(" ")?;
            }
            f.write_str("\n")?;
            let data = rows.get(kind).unwrap();
            for (_, as_to_vs) in data {
                for a in &attributes {
                    match as_to_vs.get(a) {
                        Some(value) => value.fmt(f)?,
                        None => f.write_str("_")?,
                    }
                    f.write_str(" ")?;
                }
                f.write_str("\n")?;
            }
            f.write_str("\n")?;
        }
        f.write_str("}")?;
        Result::Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
