use crate::shared::*;

impl Native {
    fn add(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Number(n1), Scalar::Number(n2)] => Ok(Value::scalar(Scalar::Number(n1 + n2))),
            [a, b] => Err(format!("{} + {}", a, b)),
            _ => unreachable!(),
        }
    }

    fn subtract(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Number(n1), Scalar::Number(n2)] => Ok(Value::scalar(Scalar::Number(n1 - n2))),
            [a, b] => Err(format!("{} - {}", a, b)),
            _ => unreachable!(),
        }
    }

    fn negative(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Number(n1)] => Ok(Value::scalar(Scalar::Number(-n1))),
            [a] => Err(format!("- {}", a)),
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

    fn is_function(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Sealed(box value)] => match value {
                Value::Set(..) => Ok(Value::nothing()),
                Value::Closure(..) => Ok(Value::something()),
            },
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
                name: "add".to_owned(),
                input_arity: 2,
                output_arity: 1,
                fun: Native::add,
            },
            Native {
                name: "subtract".to_owned(),
                input_arity: 2,
                output_arity: 1,
                fun: Native::subtract,
            },
            Native {
                name: "negative".to_owned(),
                input_arity: 1,
                output_arity: 1,
                fun: Native::negative,
            },
            Native {
                name: "permute".to_owned(),
                input_arity: 2,
                output_arity: 1,
                fun: Native::permute,
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
            Native {
                name: "is_function".to_owned(),
                input_arity: 1,
                output_arity: 1,
                fun: Native::is_function,
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
