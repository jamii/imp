// use regex::Regex;

// use relation::{ViewId, VariableId, Kind};
// use query::Query;

// struct Program{
//     views: Vec<View>,
// }

// struct View{
//     id: ViewId,
//     schema: Vec<Kind>,
//     node: Node,
// }

// // TODO terrible name, again
// enum Node {
//     Input,
//     Query(Query),
// }

// fn parse_clause(text: &str) -> (ViewId, Vec<Option<VariableId>>, Vec<Option<Kind>>) {
//     let var_re = Regex::new("\?[:alpha:]*")
//     let kind_re = Regex::new(":[:alpha:]*")
//     let bindings = text.matches(var_re).map(|var_text| {
//         match var_text {
//             "_" => None,
//             _ => Some(hash(var_text.replace(kind_re, "")))
//         }
//     }).collect();
//     let kinds = text.matches(var_re).map(|var_text| {
//         var_text.find(kind_re).map(|kind_text| {
//             match kind_text[1..] {
//                     "id" => Kind::Id,
//                     "text" => Kind::Text,
//                     "number" => Kind::Number,
//                     other => panic!("Unknown kind: {:?}", other),
//                 }
//             })
//     }).collect();
//     let view_id = hash(text.replace(var_re, "?"));
//     (view_id, bindings, kinds)
// }

// fn parse_view(text: &str) -> View {
//     let mut lines = text.split("\n").collect::<Vec<_>>;
//     let (view_id, bindings, kinds) = parse_clause(lines[0]);
//     let select = bindings.into_iter().map(|binding| binding.unwrap()).collect();
//     let schema = kinds.into_iter().map(|kind| kind.unwrap());
//     let node = match lines[1].char_at(0) {
//         "=" => {
//             // TODO handle inputs
//         }
//         "+" => {
//             // TODO handle query
//             let clauses = lines[2..].iter().map(|line| {
//                 let (view_id, bindings, kinds) = parse_clause(line);
//                 for kind in kinds.into_iter() {
//                     assert_eq!(kind, None);
//                 }
//                 Clause{view: view_id, bindings: bindings}
//             }).collect();
//             Node::Query(Query{
//                 clauses: clauses,
//                 select: select,
//             })
//         }
//     };
//     View{
//         id: view_id,
//         schema: schema,
//         node: node,
//     }
// }

// fn parse(text: &str) -> Program {
//     let mut errors = vec![];
//     let views = text.split("\n\n").map(|view_text| parse_view(view_text)).collect();
//     Program{views: view}
// }

// // TODO
// // check schemas match bindings
// // check types
// // check selects are bound