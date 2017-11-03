use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::iter::Iterator;

use language::*;

fn permuted<T: Clone>(values: &[T], ordering: &[usize]) -> Vec<T> {
    ordering.iter().map(|&ix| values[ix].clone()).collect()
}

impl Values {
    fn permuted(&self, ordering: &[usize]) -> Self {
        match self {
            &Values::Boolean(ref booleans) => Values::Boolean(permuted(booleans, ordering)),
            &Values::Integer(ref integers) => Values::Integer(permuted(integers, ordering)),
            &Values::String(ref strings) => Values::String(permuted(strings, ordering)),
            &Values::Any(ref values) => Values::Any(permuted(values, ordering)),
        }
    }
}

impl Relation {
    fn sorted(&self, ordering: &[usize]) -> Relation {
        let len = if self.columns.len() > 0 {
            self.columns[0].len()
        } else {
            0
        };
        let mut ixes = (0..len).collect::<Vec<_>>();
        for &c in ordering.iter().rev() {
            // stable sort
            ixes.sort_by(|&r1, &r2| {
                self.columns[c].get(r1).cmp(&self.columns[c].get(r2))
            });
        }
        let sorted_columns = self.columns
            .iter()
            .map(|column| column.permuted(&*ixes))
            .collect();
        Relation { columns: sorted_columns }
    }
}

// TODO can dispatch on (Values, Value) to specialize gallop

fn gallop_le(values: &Values, mut lo: usize, hi: usize, value: &Value) -> usize {
    if lo < hi && values.get(lo) < *value {
        let mut step = 1;
        while lo + step < hi && values.get(lo + step) < *value {
            lo = lo + step;
            step = step << 1;
        }

        step = step >> 1;
        while step > 0 {
            if lo + step < hi && values.get(lo + step) < *value {
                lo = lo + step;
            }
            step = step >> 1;
        }

        lo += 1
    }
    lo
}

fn gallop_leq(values: &Values, mut lo: usize, hi: usize, value: &Value) -> usize {
    if lo < hi && values.get(lo) <= *value {
        let mut step = 1;
        while lo + step < hi && values.get(lo + step) <= *value {
            lo = lo + step;
            step = step << 1;
        }

        step = step >> 1;
        while step > 0 {
            if lo + step < hi && values.get(lo + step) <= *value {
                lo = lo + step;
            }
            step = step >> 1;
        }

        lo += 1
    }
    lo
}

type LoHi = (usize, usize);

fn constrain<'a>(
    constraints: &[Constraint],
    indexes: &'a [Relation],
    ranges: &mut [LoHi],
    buffers: &mut [&mut [LoHi]],
    variables: &mut [Value<'a>],
    result_vars: &[(String, usize)],
    results: &mut Vec<Value>,
) -> Result<(), String> {
    if constraints.len() > 0 {
        let (buffer, other_buffers) = buffers.split_first_mut().unwrap();
        match &constraints[0] {
            &Constraint::Join(var_ix, result_already_fixed, ref rowcols) => {
                if result_already_fixed {
                    // loop over rowcols
                    let mut i = 0;
                    {
                        let value = &variables[var_ix];
                        while i < rowcols.len() {
                            let (row_ix, col_ix) = rowcols[i];
                            let column = &indexes[row_ix].columns[col_ix];
                            let (old_lo, old_hi) = ranges[row_ix];
                            let lo = gallop_le(column, old_lo, old_hi, value);
                            let hi = gallop_leq(column, lo, old_hi, value);
                            if lo < hi {
                                ranges[row_ix] = (lo, hi);
                                buffer[i] = (old_lo, old_hi);
                                i += 1;
                            } else {
                                break;
                            }
                        }
                    }
                    // if all succeeded, continue with rest of constraints
                    if i == rowcols.len() {
                        constrain(
                            &constraints[1..],
                            indexes,
                            ranges,
                            other_buffers,
                            variables,
                            result_vars,
                            results,
                        )?;
                    }
                    // restore state for rowcols[0..i]
                    while i > 0 {
                        i -= 1;
                        let (row_ix, _) = rowcols[i];
                        ranges[row_ix] = buffer[i];
                    }
                } else {
                    let (min_ix, &(row_ix, col_ix)) = rowcols
                        .iter()
                        .enumerate()
                        .min_by_key(|&(_, &(row_ix, _))| {
                            let (lo, hi) = ranges[row_ix];
                            hi - lo
                        })
                        .unwrap();

                    let column = &indexes[row_ix].columns[col_ix];
                    let (old_lo, old_hi) = ranges[row_ix];
                    let mut lo = old_lo;
                    // loop over rowcols[ix]
                    while lo < old_hi {
                        let value = &column.get(lo);
                        let hi = gallop_leq(column, lo + 1, old_hi, value);
                        ranges[row_ix] = (lo, hi);
                        {
                            // loop over rowcols[-ix]
                            let mut i = 0;
                            while i < rowcols.len() {
                                if i != min_ix {
                                    let (row_ix, col_ix) = rowcols[i];
                                    let column = &indexes[row_ix].columns[col_ix];
                                    let (old_lo, old_hi) = ranges[row_ix];
                                    let lo = gallop_le(column, old_lo, old_hi, value);
                                    let hi = gallop_leq(column, lo, old_hi, value);
                                    if lo < hi {
                                        ranges[row_ix] = (lo, hi);
                                        buffer[i] = (old_lo, old_hi);
                                    } else {
                                        break;
                                    }
                                }
                                i += 1;
                            }
                            // if all succeeded, continue with rest of constraints
                            if i == rowcols.len() {
                                variables[var_ix] = column.get(lo);
                                constrain(
                                    &constraints[1..],
                                    indexes,
                                    ranges,
                                    other_buffers,
                                    variables,
                                    result_vars,
                                    results,
                                )?;
                            }
                            // restore state for rowcols[-ix]
                            while i > 0 {
                                i -= 1;
                                if i != min_ix {
                                    let (row_ix, _) = rowcols[i];
                                    ranges[row_ix] = buffer[i];
                                }
                            }
                        }
                        lo = hi;
                    }
                    // restore state for rowcols[0]
                    ranges[row_ix] = (old_lo, old_hi);
                }
            }
            &Constraint::Apply(result_ix, result_already_fixed, ref function) => {
                let result = function.apply(variables)?;
                if result_already_fixed {
                    if variables[result_ix] == result {
                        constrain(
                            &constraints[1..],
                            indexes,
                            ranges,
                            other_buffers,
                            variables,
                            result_vars,
                            results,
                        )?;
                    } else {
                        // failed, backtrack
                    }
                } else {
                    variables[result_ix] = result;
                    constrain(
                        &constraints[1..],
                        indexes,
                        ranges,
                        other_buffers,
                        variables,
                        result_vars,
                        results,
                    )?;
                }
            }
        }
    } else {
        for &(_, var_ix) in result_vars.iter() {
            results.push(variables[var_ix].really_to_owned());
        }
    }
    Ok(())
}

impl Block {
    pub fn run(&self, db: &DB) -> Result<Vec<Value<'static>>, String> {
        let mut indexes: Vec<Relation> = vec![];
        let start = ::std::time::Instant::now();
        for (name, ordering) in self.row_names.iter().zip(self.row_orderings.iter()) {
            indexes.push(
                db.relations
                    .get(name)
                    .ok_or_else(|| format!("Couldn't find relation: {}", name))?
                    .sorted(ordering),
            )
        }
        let elapsed = start.elapsed();
        println!(
            "Index in {} ms",
            (elapsed.as_secs() * 1_000) + (elapsed.subsec_nanos() / 1_000_000) as u64
        );
        let mut variables: Vec<Value> = self.variables.clone();
        let mut ranges: Vec<LoHi> = indexes
            .iter()
            .map(|index| (0, index.columns[0].len()))
            .collect();
        let mut buffers: Vec<LoHi> = vec![(0, 0); indexes.len() * self.constraints.len()];
        let mut buffers: Vec<&mut [LoHi]> = buffers.chunks_mut(indexes.len()).collect();
        let mut results = vec![];
        let start = ::std::time::Instant::now();
        constrain(
            &*self.constraints,
            &*indexes,
            &mut *ranges,
            &mut *buffers,
            &mut *variables,
            &*self.result_vars,
            &mut results,
        )?;
        let elapsed = start.elapsed();
        println!(
            "Run in {} ms",
            (elapsed.as_secs() * 1_000) + (elapsed.subsec_nanos() / 1_000_000) as u64
        );
        Ok(results)
    }
}

pub fn run_code(db: &DB, code: &str, cursor: i64) {
    let code_ast = code_ast(code, cursor);

    let mut status: Vec<Result<(Block, Vec<Value>), String>> = vec![];

    // TODO bring back when output works
    // for block in code_ast.blocks.iter() {
    for block in code_ast.focused.iter().map(|ix| &code_ast.blocks[*ix]) {
        match block {
            &Err(ref error) => status.push(Err(format!("Parse error: {}", error))),
            &Ok(ref block) => {
                print!("{:?}\n\n", block);
                match plan(block) {
                    Err(error) => status.push(Err(format!("Compile error: {}", error))),
                    Ok(block) => {
                        print!("{:?}\n\n", block);
                        match block.run(db) {
                            Err(error) => status.push(Err(format!("Run error: {}", error))),
                            Ok(results) => {
                                status.push(Ok((block, results)));
                            }
                        }
                    }
                }
            }
        }
    }

    if let Some(ix) = code_ast.focused {
        // TODO bring back when output works
        // match &status[ix] {
        match &status[0] {
            &Err(ref error) => print!("{}\n\n", error),
            &Ok((ref block, ref results)) => {
                let result_vars = &block.result_vars;

                print!(
                    "Ok: {} results\n\n",
                    if result_vars.len() > 0 {
                        results.len() / result_vars.len()
                    } else {
                        0
                    }
                );

                if result_vars.len() > 0 {
                    for (i, row) in results.chunks(result_vars.len()).take(10).enumerate() {
                        for (&(ref name, _), value) in result_vars.iter().zip(row.iter()) {
                            print!("{}={}\t", name, value);
                        }
                        if i == 9 {
                            print!("...\n");
                        } else {
                            print!("\n");
                        }
                    }
                    print!("\n");
                }

                print!("{:?}\n\n{:?}\n\n", code_ast.blocks[ix], block);
            }
        }

    } else {
        print!("Nothing focused\n\n");
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum EditorEvent {
    State(String, i64),
}

// #[derive(Debug, Serialize, Deserialize)]
// enum Command {
//     Render(String),
// }

// fn send_command(sender: &mut websocket::sender::Writer<std::net::TcpStream>, c: Command) {
//     sender
//         .send_message(&OwnedMessage::Text(json!(c).to_string()))
//         .unwrap()
// }

pub fn serve_editor(db: DB) {
    println!("Tables: {:?}", db.relations.keys().collect::<Vec<_>>());

    let state = Arc::new(Mutex::new(("".to_owned(), 0)));

    let server = Server::bind("127.0.0.1:8081").unwrap();

    thread::spawn({
        let state = state.clone();
        move || {
            let mut last_state = state.lock().unwrap().clone();
            loop {
                let state: (String, i64) = state.lock().unwrap().clone();
                if state != last_state {
                    print!("\x1b[2J\x1b[1;1H");
                    let (ref code, cursor) = state;
                    let start = ::std::time::Instant::now();
                    run_code(&db, &*code, cursor);
                    let elapsed = start.elapsed();
                    println!(
                        "In {} ms",
                        (elapsed.as_secs() * 1_000) + (elapsed.subsec_nanos() / 1_000_000) as u64
                    );
                    last_state = state.clone();
                }
            }
        }
    });

    for request in server.filter_map(Result::ok) {
        let state = state.clone();
        thread::spawn(move || {
            let client = request.accept().unwrap();
            let ip = client.peer_addr().unwrap();
            println!("Connection from {}", ip);

            let (mut receiver, mut sender) = client.split().unwrap();

            for message in receiver.incoming_messages() {
                let message = message.unwrap();

                match message {
                    OwnedMessage::Close(_) => {
                        let message = OwnedMessage::Close(None);
                        sender.send_message(&message).unwrap();
                        println!("Client {} disconnected", ip);
                        return;
                    }
                    OwnedMessage::Ping(ping) => {
                        let message = OwnedMessage::Pong(ping);
                        sender.send_message(&message).unwrap();
                    }
                    OwnedMessage::Text(ref text) => {
                        // println!("Received: {}", text);
                        let event: EditorEvent = ::serde_json::from_str(text).unwrap();
                        match event {
                            EditorEvent::State(code, cursor) => {
                                *state.lock().unwrap() = (code, cursor);
                            }
                        }
                    }
                    _ => {
                        panic!("A weird message! {:?}", message);
                    }
                }
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let col = Values::Integer((0..1000).collect());
        for i in 0..1000 {
            assert_eq!(
                col.get(gallop_le(&col, 0, 1000, &Value::Integer(i))),
                Value::Integer(i)
            );
        }
        assert_eq!(gallop_le(&col, 0, col.len(), &Value::Integer(-1)), 0);
        assert_eq!(gallop_le(&col, 0, col.len(), &Value::Integer(1000)), 1000);

        for i in 0..999 {
            assert_eq!(
                col.get(gallop_leq(&col, 0, col.len(), &Value::Integer(i))),
                Value::Integer(i + 1)
            );
        }
        assert_eq!(gallop_leq(&col, 0, col.len(), &Value::Integer(-1)), 0);
        assert_eq!(gallop_leq(&col, 0, col.len(), &Value::Integer(999)), 1000);
    }
}
