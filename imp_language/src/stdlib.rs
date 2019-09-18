use crate::shared::*;

impl Native {
    fn add(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Number(n1), Scalar::Number(n2)] => Ok(Value::scalar(Scalar::Number(n1 + n2))),
            [a, b] => Err(format!("{} + {}", a, b)),
            _ => unreachable!(),
        }
    }

    fn permute(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Sealed(box Value::Set(permutations)), Scalar::Sealed(box Value::Set(set))] => {
                let mut result = Set::new();
                for permutation in permutations {
                    for row in set {
                        result.insert(
                            permutation
                                .iter()
                                .map(|i| {
                                    let i = i.as_integer()? - 1;
                                    if 0 <= i && i < (row.len() as i64) {
                                        Ok(row[i as usize].clone())
                                    } else {
                                        Err(format!("Out of bounds: {}", i + 1))
                                    }
                                })
                                .collect::<Result<Vec<_>, _>>()?,
                        );
                    }
                }
                Ok(Value::Set(result))
            }
            [a, b] => Err(format!("permute {} {}", a, b)),
            _ => unreachable!(),
        }
    }

    fn reduce(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [init, Scalar::Sealed(box fun), Scalar::Sealed(box Value::Set(set))] => {
                let mut result = Value::Set(Set::from_iter(vec![vec![init.clone()]]));
                for row in set {
                    let env = Environment::from(vec![
                        ("fun".to_owned(), fun.clone()),
                        ("result".to_owned(), result),
                        (
                            "row".to_owned(),
                            Value::Set(Set::from_iter(vec![row.clone()])),
                        ),
                    ]);
                    let expr = Expression::Apply(
                        box Expression::Apply(
                            box Expression::Name("fun".to_owned()),
                            box Expression::Name("result".to_owned()),
                        ),
                        box Expression::Name("row".to_owned()),
                    );
                    result = expr.eval(&env)?;
                }
                Ok(result)
            }
            [a, b, c] => Err(format!("reduce {} {} {}", a, b, c)),
            _ => unreachable!(),
        }
    }

    // TODO pivot isn't quite the right name for this
    fn pivot(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Sealed(box Value::Set(set))] => {
                let mut result = Set::new();
                let mut input = set.iter().collect::<Vec<_>>();
                input.sort();
                for (r, row) in input.into_iter().enumerate() {
                    for (c, val) in row.into_iter().enumerate() {
                        result.insert(vec![
                            Scalar::Number((r + 1) as i64),
                            Scalar::Number((c + 1) as i64),
                            val.clone(),
                        ]);
                    }
                }
                Ok(Value::Set(result))
            }
            [a] => Err(format!("pivot {}", a)),
            _ => unreachable!(),
        }
    }

    fn as_text(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [scalar] => Ok(Value::Set(Set::from_iter(vec![vec![Scalar::String(
                format!("{}", scalar),
            )]]))),
            _ => unreachable!(),
        }
    }

    // fn solve(scalars: Vec<Scalar>) -> Result<Value, String> {
    //     match &*scalars {
    //         [Scalar::Sealed(box expr, env)] => {
    //             // TODO scalar_env?
    //             let scalar_env = Environment::from(
    //                 env.bindings
    //                     .iter()
    //                     .map(|(name, _value)| (name.clone(), false))
    //                     .collect::<Vec<(String, bool)>>(),
    //             );
    //             let type_env = Environment::from(
    //                 env.bindings
    //                     .iter()
    //                     .map(|(name, value)| Ok((name.clone(), value.typ()?)))
    //                     .collect::<Result<Vec<(String, ValueType)>, String>>()?,
    //             );
    //             let mut scalar_cache = Cache::new();
    //             let mut type_cache = Cache::new();
    //             expr.scalar(&scalar_env, &mut scalar_cache)?;
    //             expr.typecheck(&type_env, &mut type_cache)?;
    //             let lowered = expr.lower(&scalar_cache, &type_cache)?;
    //             Ok(Value::Set(Set::from_iter(vec![vec![Scalar::Sealed(
    //                 box lowered,
    //                 env.clone(),
    //             )]])))
    //         }
    //         [a] => Err(format!("solve {}", a)),
    //         _ => unreachable!(),
    //     }
    // }

    pub fn stdlib() -> Vec<Native> {
        vec![
            Native {
                name: "+".to_owned(),
                input_arity: 2,
                output_arity: 1,
                fun: Native::add,
            },
            Native {
                name: "permute".to_owned(),
                input_arity: 2,
                output_arity: 1,
                fun: Native::permute,
            },
            Native {
                name: "reduce".to_owned(),
                input_arity: 3,
                output_arity: 1,
                fun: Native::reduce,
            },
            Native {
                name: "pivot".to_owned(),
                input_arity: 1,
                output_arity: 3,
                fun: Native::pivot,
            },
            Native {
                name: "as_text".to_owned(),
                input_arity: 1,
                output_arity: 1,
                fun: Native::as_text,
            },
            // Native {
            //     name: "solve".to_owned(),
            //     input_arity: 1,
            //     output_arity: 1,
            //     fun: Native::solve,
            // },
        ]
    }
}