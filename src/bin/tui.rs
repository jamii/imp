extern crate imp;

use imp::*;

fn main() {
  let mut data = Data::new();
  let mut person = Kind{name: "Person".into(), attributes: vec![]};
  let mut national_insurance = Attribute{name: "national insurance".into, kind: person.clone()};
  // TODO tie the knot
  data.add_e()
  println!("{:?}", data);
}