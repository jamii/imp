use std::error::Error;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, BufReader};

use language::*;

fn parse_chinook() -> Result<DB, Box<Error>> {
    println!("Parsing chinook");
    let mut relations: HashMap<String, Relation> = HashMap::new();
    for (name, kinds) in vec![
        ("Album", vec![Kind::Integer, Kind::String, Kind::Integer]),
        // "Customer",
        // "Genre",
        // "InvoiceLine",
        // "MediaType",
        // "PlaylistTrack",
        ("Artist", vec![Kind::Integer, Kind::String]),
        // "Employee",
        // "Invoice",
        // "Playlist",
        // "Track",
    ]
    {
        let mut reader = ::csv::ReaderBuilder::new().delimiter(b'\t').from_reader(
            File::open(
                format!(
                    "./data/{}.csv",
                    name
                ),
            )?,
        );
        let mut columns: Vec<Values> = kinds.iter().map(|kind| Values::new(kind)).collect();
        for record_or_error in reader.records() {
            let record = record_or_error?;
            for (c, (field, kind)) in record.iter().zip(kinds.iter()).enumerate() {
                columns[c].push(kind.parse(field)?);
            }
        }
        relations.insert(name.to_lowercase(), Relation { columns });
    }
    println!("Parsing chinook done");
    Ok(DB { relations })
}

fn parse_imdb() -> Result<DB, String> {
    println!("Parsing imdb");
    let mut relations: HashMap<String, Relation> = HashMap::new();
    let mut schema_reader = ::csv::ReaderBuilder::new().delimiter(b',').from_reader(
        File::open(
            "data/job_schema.csv",
        ).map_err(|e| {
            format!("data/job_schema.csv: {:?}", e)
        })?,
    );
    for (line, record_or_error) in schema_reader.records().enumerate() {
        let record = record_or_error.map_err(|e| {
            format!("data/job_schema.csv line {}: {:?}", line + 1, e)
        })?;
        let table_name = &record[0];
        let ix = record[1].parse::<usize>().unwrap() - 1; // schema is 1-indexed
        let column_name = &record[2];
        let column_type = &record[3];
        match File::open(format!("../imdb/pg_{}.csv", table_name)) {
            Err(_) => (), // don't have data for this file
            Ok(file) => {
                let name = format!("{}.{}", table_name, column_name);
                println!("Parsing imdb: {}", name);
                let mut data_reader = ::csv::ReaderBuilder::new().delimiter(b',').from_reader(
                    file,
                );
                let key_kind = Kind::Integer;
                let val_kind = match &*column_type {
                    "integer" => Kind::Integer,
                    _ => Kind::String,
                };
                let mut columns = vec![Values::new(&key_kind), Values::new(&val_kind)];
                for (line, record_or_error) in data_reader.records().enumerate() {
                    let record = record_or_error.map_err(|e| {
                        format!("{} line {}: {:?}", name, line + 1, e)
                    })?;
                    if &record[ix] != "" {
                        columns[0].push(key_kind.parse(&record[0]).map_err(|e| {
                            format!("{} line {}: {}", name, line + 1, e)
                        })?);
                        columns[1].push(val_kind.parse(&record[ix]).map_err(|e| {
                            format!("{} line {}: {}", name, line + 1, e)
                        })?);
                    }
                }
                relations.insert(name, Relation { columns });
            }
        }
    }
    println!("Parsing imdb finished");

    Ok(DB { relations })
}

fn save_db(filename: &str, db: &DB) {
    println!("Saving to {}", filename);
    let mut file = BufWriter::new(File::create(filename).unwrap());
    ::bincode::serialize_into(&mut file, db, ::bincode::Infinite).unwrap();
    println!("Saving done");
}

fn load_db(filename: &str) -> DB {
    println!("Loading from {}", filename);
    let mut file = BufReader::new(File::open(filename).unwrap());
    let db = ::bincode::deserialize_from(&mut file, ::bincode::Infinite).unwrap();
    println!("Loading done");
    db
}

pub fn import() {
    save_db("../chinook.bincode", &parse_chinook().unwrap());
    save_db("../imdb.bincode", &parse_imdb().unwrap());
}

pub fn load_chinook() -> DB {
    load_db("../chinook.bincode")
}

pub fn load_imdb() -> DB {
    load_db("../imdb.bincode")
}
