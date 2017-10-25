use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::iter::Iterator;

use std::borrow::Cow;
use std::borrow::Borrow;

use language::*;

// f should be `|t| t < value` or `|t| t <= value`
fn gallop<'a, T, F: Fn(&T) -> bool>(slice: &'a [T], mut lo: usize, hi: usize, f: F) -> usize {
    if lo < hi && f(&slice[lo]) {
        let mut step = 1;
        while lo + step < hi && f(&slice[lo + step]) {
            lo = lo + step;
            step = step << 1;
        }

        step = step >> 1;
        while step > 0 {
            if lo + step < hi && f(&slice[lo + step]) {
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
    indexes: &'a [[Vec<Value>; 3]],
    ranges: &mut [LoHi],
    variables: &mut [Cow<'a, Value>],
    results: &mut Vec<Value>,
    asserts: &mut Vec<[Value; 3]>,
) -> Result<(), String> {
    if constraints.len() > 0 {
        match &constraints[0] {
            &Constraint::Join(var_ix, result_already_fixed, ref rowcols) => {
                let mut buffer = vec![(0, 0); rowcols.len()]; // TODO pre-allocate
                if result_already_fixed {
                    // loop over rowcols[0..]
                    let mut i = 0;
                    {
                        let value = variables[var_ix].borrow();
                        while i < rowcols.len() {
                            let (row_ix, col_ix) = rowcols[i];
                            let column = &indexes[row_ix][col_ix];
                            let (old_lo, old_hi) = ranges[row_ix];
                            let lo = gallop(column, old_lo, old_hi, |v| v < value);
                            let hi = gallop(column, lo, old_hi, |v| v <= value);
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
                            variables,
                            results,
                            asserts,
                        )?;
                    }
                    // restore state for rowcols[0..i]
                    while i > 0 {
                        i -= 1;
                        let (row_ix, _) = rowcols[i];
                        ranges[row_ix] = buffer[i];
                    }
                } else {
                    let (row_ix, col_ix) = rowcols[0]; // TODO pick smallest
                    let column = &indexes[row_ix][col_ix];
                    let (old_lo, old_hi) = ranges[row_ix];
                    let mut lo = old_lo;
                    // loop over rowcols[0]
                    while lo < old_hi {
                        let value = &column[lo];
                        let hi = gallop(column, lo + 1, old_hi, |v| v <= value);
                        ranges[row_ix] = (lo, hi);
                        {
                            // loop over rowcols[1..]
                            let mut i = 1;
                            while i < rowcols.len() {
                                let (row_ix, col_ix) = rowcols[i];
                                let column = &indexes[row_ix][col_ix];
                                let (old_lo, old_hi) = ranges[row_ix];
                                let lo = gallop(column, old_lo, old_hi, |v| v < value);
                                let hi = gallop(column, lo, old_hi, |v| v <= value);
                                if lo < hi {
                                    ranges[row_ix] = (lo, hi);
                                    buffer[i] = (old_lo, old_hi);
                                    i += 1;
                                } else {
                                    break;
                                }
                            }
                            // if all succeeded, continue with rest of constraints
                            if i == rowcols.len() {
                                variables[var_ix] = Cow::Borrowed(&column[lo]);
                                constrain(
                                    &constraints[1..],
                                    indexes,
                                    ranges,
                                    variables,
                                    results,
                                    asserts,
                                )?;
                            }
                            // restore state for rowcols[1..i]
                            while i > 1 {
                                i -= 1;
                                let (row_ix, _) = rowcols[i];
                                ranges[row_ix] = buffer[i];
                            }
                        }
                        lo = hi;
                    }
                    // restore state for rowcols[0]
                    ranges[row_ix] = (old_lo, old_hi);
                }
            }
            &Constraint::Apply(result_ix, result_already_fixed, ref function) => {
                let result = Cow::Owned(function.apply(variables)?);
                if result_already_fixed {
                    if variables[result_ix] == result {
                        constrain(
                            &constraints[1..],
                            indexes,
                            ranges,
                            variables,
                            results,
                            asserts,
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
                        variables,
                        results,
                        asserts,
                    )?;
                }
            }
            &Constraint::Assert(var_ixes) => {
                {
                    let v0: &Value = variables[var_ixes[0]].borrow();
                    let v1: &Value = variables[var_ixes[1]].borrow();
                    let v2: &Value = variables[var_ixes[2]].borrow();
                    asserts.push([v0.to_owned(), v1.to_owned(), v2.to_owned()]);
                }
                constrain(
                    &constraints[1..],
                    indexes,
                    ranges,
                    variables,
                    results,
                    asserts,
                )?;
            }
            &Constraint::Debug(ref named_variables) => {
                for &(_, var_ix) in named_variables.iter() {
                    results.push(variables[var_ix].clone().into_owned());
                }
                constrain(
                    &constraints[1..],
                    indexes,
                    ranges,
                    variables,
                    results,
                    asserts,
                )?;
            }
        }
    }
    Ok(())
}

impl Block {
    fn run(&self, bag: &Bag) -> Result<(Vec<Value>, Vec<[Value; 3]>), String> {
        // TODO strip out this compat layer
        let eavs: Vec<[Value; 3]> = bag.eavs
            .iter()
            .map(|(&(ref e, ref a), v)| {
                [
                    Value::Entity(e.clone()),
                    Value::String(a.clone()),
                    v.clone(),
                ]
            })
            .collect();
        let indexes: Vec<[Vec<Value>; 3]> = self.row_orderings
            .iter()
            .map(|row_ordering| {
                let mut ordered_eavs: Vec<[Value; 3]> = eavs.iter()
                    .map(|eav| {
                        [
                            eav[row_ordering[0]].clone(),
                            eav[row_ordering[1]].clone(),
                            eav[row_ordering[2]].clone(),
                        ]
                    })
                    .collect();
                ordered_eavs.sort_unstable();
                let mut reverse_ordering = [0, 0, 0];
                for i in 0..3 {
                    reverse_ordering[row_ordering[i]] = i;
                }
                [
                    ordered_eavs
                        .iter()
                        .map(|eav| eav[reverse_ordering[0]].clone())
                        .collect(),
                    ordered_eavs
                        .iter()
                        .map(|eav| eav[reverse_ordering[1]].clone())
                        .collect(),
                    ordered_eavs
                        .iter()
                        .map(|eav| eav[reverse_ordering[2]].clone())
                        .collect(),
                ]
            })
            .collect();
        let mut variables: Vec<Cow<Value>> =
            self.variables.iter().map(|v| Cow::Borrowed(v)).collect();
        let mut ranges: Vec<LoHi> = indexes.iter().map(|index| (0, index[0].len())).collect();
        let mut results = vec![];
        let mut asserts = vec![];
        constrain(
            &*self.constraints,
            &*indexes,
            &mut *ranges,
            &mut *variables,
            &mut results,
            &mut asserts,
        )?;
        Ok((results, asserts))
    }
}

pub fn run_code(bag: &mut Bag, code: &str, cursor: i64) {
    let code_ast = code_ast(code, cursor);
    let mut status: Vec<Result<(Block, Vec<Value>, Vec<[Value; 3]>), String>> = vec![];
    for block in code_ast.blocks.iter() {
        if let Some(&Err(ref error)) = block.statements.iter().find(|s| s.is_err()) {
            status.push(Err(format!("Parse error: {}", error)));
        } else {
            match plan(block) {
                Err(error) => status.push(Err(format!("Compile error: {}", error))),
                Ok(block) => {
                    match block.run(&bag) {
                        Err(error) => status.push(Err(format!("Run error: {}", error))),
                        Ok((results, asserts)) => {
                            for &[ref e, ref a, ref v] in asserts.iter() {
                                bag.insert((e.clone(), a.clone(), v.clone()));
                            }
                            status.push(Ok((block, results, asserts)));
                        }
                    }
                }
            }
        }
    }

    if let Some(ix) = code_ast.focused {
        match &status[ix] {
            &Err(ref error) => print!("{}\n\n", error),
            &Ok((ref block, ref results, ref asserts)) => {
                if let Some(&Constraint::Debug(ref named_variables)) = block.constraints.last() {

                    print!(
                        "Ok: {} results, {} asserts\n\n",
                        results.len() / named_variables.len(),
                        asserts.len()
                    );

                    if named_variables.len() > 0 {
                        for (i, row) in results.chunks(named_variables.len()).take(10).enumerate() {
                            for (&(ref name, _), value) in named_variables.iter().zip(row.iter()) {
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
                }

                for assert in asserts.iter() {
                    print!(
                        "+ {}.{} = {}\n",
                        assert[0],
                        assert[1].as_str().unwrap(),
                        assert[2]
                    );
                }
                print!("\n");

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

pub fn serve_editor() {
    let state = Arc::new(Mutex::new(("".to_owned(), 0)));

    let server = Server::bind("127.0.0.1:8081").unwrap();

    thread::spawn({
        let state = state.clone();
        move || {
            let bag = chinook().unwrap();
            println!("Bag is {:?}", bag);
            let mut last_state = state.lock().unwrap().clone();
            loop {
                let state = &*state.lock().unwrap();
                if *state != last_state {
                    print!("\x1b[2J\x1b[1;1H");
                    let &(ref code, cursor) = state;
                    let start = ::std::time::Instant::now();
                    run_code(&mut bag.clone(), &*code, cursor);
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
        let col = (0..1000).collect::<Vec<_>>();
        for i in 0..1000 {
            assert_eq!(col[gallop(&*col, 0, 1000, |&x| x < i)], i);
        }
        assert_eq!(gallop(&*col, 0, col.len(), |&x| x < -1), 0);
        assert_eq!(gallop(&*col, 0, col.len(), |&x| x < 1000), 1000);

        for i in 0..999 {
            assert_eq!(col[gallop(&*col, 0, col.len(), |&x| x <= i)], i + 1);
        }
        assert_eq!(gallop(&*col, 0, col.len(), |&x| x <= -1), 0);
        assert_eq!(gallop(&*col, 0, col.len(), |&x| x <= 999), 1000);
    }
}
