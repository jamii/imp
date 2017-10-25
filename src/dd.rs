use std::collections::HashMap;
use std::iter::Iterator;

// use timely::dataflow::*;
use timely::dataflow::operators::*;

use differential_dataflow::Collection;
use differential_dataflow::operators::*;

use differential_dataflow::operators::arrange::ArrangeByKey;

use abomonation::Abomonation;

use language::*;

unsafe_abomonate!(Entity: avs);

impl Abomonation for Value {
    #[inline]
    unsafe fn embalm(&mut self) {
        match self {
            &mut Value::Boolean(ref mut inner) => inner.embalm(),
            &mut Value::Integer(ref mut inner) => inner.embalm(),
            &mut Value::String(ref mut inner) => inner.embalm(),
            &mut Value::Entity(ref mut inner) => inner.embalm(),
        }
    }

    #[inline]
    unsafe fn entomb(&self, bytes: &mut Vec<u8>) {
        match self {
            &Value::Boolean(ref inner) => inner.entomb(bytes),
            &Value::Integer(ref inner) => inner.entomb(bytes),
            &Value::String(ref inner) => inner.entomb(bytes),
            &Value::Entity(ref inner) => inner.entomb(bytes),
        }
    }

    #[inline]
    unsafe fn exhume<'a, 'b>(&'a mut self, bytes: &'b mut [u8]) -> Option<&'b mut [u8]> {
        match self {
            &mut Value::Boolean(ref mut inner) => inner.exhume(bytes),
            &mut Value::Integer(ref mut inner) => inner.exhume(bytes),
            &mut Value::String(ref mut inner) => inner.exhume(bytes),
            &mut Value::Entity(ref mut inner) => inner.exhume(bytes),
        }
    }
}

fn get_all(row: &[Value], key: &[usize]) -> Vec<Value> {
    key.iter().map(|&ix| row[ix].clone()).collect()
}

pub fn serve_dataflow() {
    let code = ::std::env::args().skip(1).next().unwrap();
    println!("Running:\n{}", code);
    ::timely::execute_from_args(::std::env::args().skip(1), move |worker| {

        let peers = worker.peers();
        let index = worker.index();
        println!("peers {:?} index {:?}", peers, index);

        let eavs = chinook()
            .unwrap()
            .eavs
            .into_iter()
            .map(|((e, a), v)| {
                (
                    vec![Value::Entity(e), Value::String(a), v],
                    Default::default(),
                    1,
                )
            })
            .collect::<Vec<_>>();

        let code = code.clone();
        worker.dataflow::<(), _, _>(move |scope| {
            let mut eavs: Collection<_, Vec<Value>, _> = Collection::new(eavs.to_stream(scope));

            let code_ast = code_ast(&*code, 0);

            for block_ast in code_ast.blocks.iter() {
                assert!(block_ast.statements.iter().all(|s| s.is_ok()));
                let block = plan(block_ast).unwrap();
                println!("{:?}", block);

                let mut rc_var: HashMap<RowCol, usize> = HashMap::new();
                
                let mut variables: Collection<_, Vec<Value>, _> =
                    Collection::new(
                        vec![(block.variables.clone(), Default::default(), 1)].to_stream(scope),
                    );
                let mut asserts: Collection<_, Vec<Value>, _> = Collection::new(vec![].to_stream(scope));
                let _ = vec![&variables, &asserts]; // hacky way to assert that they both have the same type, rather than filling in the _ on asserts
                for constraint in block.constraints.iter() {
                    match constraint {
                        &Constraint::Join(var, result_already_fixed, ref rcs) => {
                            let mut result_already_fixed = result_already_fixed;
                            for &(r, c) in rcs.iter() {
                                let r = r.clone();
                                let c = c.clone();
                                let mut variables_key = vec![];
                                let mut eav_key = vec![];
                                for c2 in 0..3 {
                                    if (c2 == c) && result_already_fixed {
                                        variables_key.push(var);
                                        eav_key.push(c);
                                    }
                                    if let Some(&var2) = rc_var.get(&(r, c2)) {
                                        variables_key.push(var2);
                                        eav_key.push(c2);
                                    }
                                }
                                let index = eavs
                                    .map(move |row| (get_all(&*row, &*eav_key), row))
                                    .arrange_by_key();
                                variables = variables
                                    .map(move |row| (get_all(&*row, &*variables_key), row))
                                    .arrange_by_key()
                                    .join_core(&index, move |_key, row, eav| {
                                        let mut row = row.clone();
                                        row[var] = eav[c].clone();
                                        vec![row]
                                    });
                                result_already_fixed = true;
                                rc_var.insert((r, c), var);
                            }
                        }
                        &Constraint::Apply(var, result_already_fixed, ref function) => {
                            let var = var.clone();
                            let function = function.clone();
                            if result_already_fixed {
                                variables = variables.filter(move |row| {
                                    let result = function.apply(&*row).unwrap();
                                    row[var] == result
                                });
                            } else {
                                variables = variables.map(move |mut row| {
                                    let result = function.apply(&*row).unwrap();
                                    row[var] = result;
                                    row
                                });
                            }
                        }
                        &Constraint::Assert(ref vars) => {
                            let vars = vars.clone();
                            asserts = asserts.concat(&variables.map(move |row| get_all(&*row, &vars)));
                        }
                        &Constraint::Debug(ref names_and_vars) => {
                            let names_and_vars = names_and_vars.clone();
                            variables.inspect(move |&(ref row, _, _)| {
                                let mut output = String::new();
                                for &(ref name, var) in names_and_vars.iter() {
                                    output.push_str(&*format!("{}={}\t", name, row[var]));
                                }
                                println!("{}", output);
                            });
                        }
                    }
                }

                asserts.inspect(|&(ref row, _, _)| {
                    println!("+ {}.{} = {}", row[0], row[1].as_str().unwrap(), row[2]);
                });

                eavs = eavs.concat(&asserts);
            }
        });

    }).unwrap();
}