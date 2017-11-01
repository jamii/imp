use std::error::Error;
use std::collections::HashMap;
use std::fs::File;

use language::*;

pub fn load_chinook() -> Result<DB, Box<Error>> {
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
    Ok(DB { relations })
}

pub fn load_imdb() -> Result<DB, Box<Error>> {
    println!("Loading IMDB");
    let mut relations: HashMap<String, Relation> = HashMap::new();
    let mut schema_reader = ::csv::ReaderBuilder::new().delimiter(b',').from_reader(
        File::open(
            "data/job_schema.csv",
        )?,
    );
    for record_or_error in schema_reader.records() {
        let record = record_or_error?;
        let table_name = &record[0];
        let ix = record[1].parse::<usize>().unwrap();
        let column_name = &record[2];
        let column_type = &record[3];
        println!("Loading IMDB: {}.{}", table_name, column_name);
        match File::open(format!("../imdb/pg_{}.csv", table_name)) {
            Err(_) => (), // don't have data for this file
            Ok(file) => {
                let mut data_reader = ::csv::ReaderBuilder::new().delimiter(b',').from_reader(
                    file,
                );
                let key_kind = Kind::Integer;
                let val_kind = match &*column_type {
                    "integer" => Kind::Integer,
                    _ => Kind::String,
                };
                let mut columns = vec![Values::new(&key_kind), Values::new(&val_kind)];
                for record_or_error in data_reader.records() {
                    let record = record_or_error?;
                    columns[0].push(key_kind.parse(&record[0])?);
                    columns[1].push(val_kind.parse(&record[ix])?);
                }
                let name = format!("{}.{}", table_name, column_name);
                relations.insert(name, Relation { columns });
            }
        }
    }
    println!("Loading IMDB finished");
    Ok(DB { relations })
}
