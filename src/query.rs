use chunk::*;

#[derive(Clone, Debug)]
pub enum Action {
    Sort(usize, Vec<usize>),
    Project(usize, Vec<usize>),
    SemiJoin(usize, usize, Vec<usize>, Vec<usize>),
    Join(usize, usize, Vec<usize>, Vec<usize>),
    DebugChunk(usize),
    DebugText(usize, usize),
}

#[derive(Clone, Debug)]
pub struct Plan {
    pub actions: Vec<Action>,
    pub result: usize,
}

impl Plan {
    pub fn execute(&self, strings: &Vec<String>, mut chunks: Vec<Chunk>) -> Chunk {
        for action in self.actions.iter() {
            // println!("");
            // println!("{:?}", chunks.iter().map(|chunk| chunk.len()).collect::<Vec<_>>());
            // println!("{:?}", action);
            // time!(format!("{:?}", action), {
            match action {
                &Action::Sort(ix, ref key) => {
                    let chunk = chunks[ix].sort(&key[..]);
                    chunks[ix] = chunk;
                },
                &Action::Project(ix, ref key) => {
                    let chunk = chunks[ix].project(&key[..]);
                    chunks[ix] = chunk;
                }
                &Action::SemiJoin(left_ix, right_ix, ref left_key, ref right_key) => {
                    let (left_chunk, right_chunk) = chunks[left_ix].semijoin(&chunks[right_ix], &left_key[..], &right_key[..]);
                    chunks[left_ix] = left_chunk;
                    chunks[right_ix] = right_chunk;
                },
                &Action::Join(left_ix, right_ix, ref left_key, ref right_key) => {
                    let chunk = chunks[left_ix].join(&chunks[right_ix], &left_key[..], &right_key[..]);
                    chunks[left_ix] = Chunk::empty();
                    chunks[right_ix] = chunk;
                }
                &Action::DebugChunk(ix) => {
                    let chunk = &chunks[ix];
                    println!("{:?}", chunk.data.chunks(chunk.row_width).collect::<Vec<_>>());
                }
                &Action::DebugText(ix, field) => {
                    let chunk = &chunks[ix];
                    println!("{:?}", chunk.data.chunks(chunk.row_width).map(|row| &strings[row[field] as usize]).collect::<Vec<_>>());
                }
            }
            // });
        }
        ::std::mem::replace(&mut chunks[self.result], Chunk::empty())
    }
}

#[cfg(test)]
mod tests{
    use super::*;
    use chunk::Chunk;
    use relation::Kind;
    use rand::{Rng, SeedableRng, StdRng};
    use test::{Bencher, black_box};
    use std::io::prelude::*;
    use std::fs::File;
    use std::hash::{Hash, Hasher, SipHasher};

    fn hash<T: Hash>(t: &T) -> u64 {
        let mut s = SipHasher::new();
        t.hash(&mut s);
        s.finish()
    }

    fn from_tsv(filename: &'static str, kinds: Vec<Kind>, strings: &mut Vec<String>) -> Chunk {
        let mut tsv = String::new();
        File::open(filename).unwrap().read_to_string(&mut tsv);
        let mut lines = tsv.lines();
        lines.next(); // drop header
        let mut data = vec![];
        for line in lines {
            for (kind, field) in kinds.iter().zip(line.split("\t")) {
                match *kind {
                    Kind::Id => data.push(field.parse::<u64>().unwrap()),
                    Kind::Number => data.push(field.parse::<f64>().unwrap() as u64),
                    Kind::Text => {
                        let field = field.to_owned();
                        data.push(hash(&field));
                        data.push(strings.len() as u64);
                        strings.push(field);
                    }
                }
            }
        }
        let row_width = kinds.iter().map(|kind| kind.width()).sum();
        Chunk{data: data, row_width: row_width}
    }

    fn chinook() -> (Vec<String>, Vec<Chunk>) {
        use relation::Kind::*;
        let mut strings = vec![];
        let chunks = vec![
            from_tsv("data/Artist.csv", vec![Id, Text], &mut strings),
            from_tsv("data/Album.csv", vec![Id, Text, Id], &mut strings),
            from_tsv("data/Track.csv", vec![Id, Text, Id, Id, Id, Text, Number, Number, Number], &mut strings),
            from_tsv("data/PlaylistTrack.csv", vec![Id, Id], &mut strings),
            from_tsv("data/Playlist.csv", vec![Id, Text], &mut strings),
        ];
        (strings, chunks)
    }

    #[test]
    fn test_chinook() {
        chinook();
    }

    fn chinook_metal(mut strings: Vec<String>, mut chunks: Vec<Chunk>) -> Chunk {
        use query::Action::*;
        let metal = "Heavy Metal Classic".to_owned();
        let query = Chunk{ data: vec![hash(&metal), strings.len() as u64], row_width: 2};
        strings.push(metal);
        chunks.push(query);
        let plan = Plan{
            actions: vec![
            // semijoin Query and Playlist on Name
            Sort(5, vec![0]),
            Sort(4, vec![1]),
            SemiJoin(5, 4, vec![0], vec![1]),

            // semijoin Playlist and PlaylistTrack on PlaylistId
            Sort(4, vec![0]),
            Sort(3, vec![0]),
            SemiJoin(4, 3, vec![0], vec![0]),

            // semijoin PlaylistTrack and Track on TrackId
            Sort(3, vec![1]),
            Project(2, vec![0, 3]),
            SemiJoin(3, 2, vec![1], vec![0]),

            // semijoin Track and Album on AlbumId
            Sort(2, vec![1]),
            Project(1, vec![0, 3]),
            SemiJoin(2, 1, vec![1], vec![0]),

            // join Artist and Album on ArtistId
            Sort(0, vec![0]),
            Sort(1, vec![1]),
            Join(0, 1, vec![0], vec![1]),

            // join AlbumId/Name and Track on AlbumId
            Project(1, vec![3, 1, 2]),
            Join(1, 2, vec![0], vec![1]),

            // join TrackId/Name and PlaylistTrack on TrackId
            Project(2, vec![3, 1, 2]),
            Join(2, 3, vec![0], vec![1]),

            // join PlaylistId/Name and Playlist on PlaylistId
            Project(3, vec![3, 1, 2]),
            Join(3, 4, vec![0], vec![0]),

            // join Name/Name and Query on Name
            Project(4, vec![4, 5, 1, 2]),
            Join(4, 5, vec![0], vec![0]),

            // project Name without hash
            Project(5, vec![3]),
            ],

            result: 5
        };
        plan.execute(&strings, chunks)
    }

    #[test]
    fn test_chinook_metal() {
        let (strings, chunks) = chinook();
        let results = chinook_metal(strings.clone(), chunks);
        assert_eq!(
            results.data.iter().map(|ix| &strings[*ix as usize]).collect::<Vec<_>>(),
            vec!["AC/DC", "Accept", "Black Sabbath", "Metallica", "Iron Maiden", "Mot\u{f6}rhead", "M\u{f6}tley Cr\u{fc}e", "Ozzy Osbourne", "Scorpions"]
            );
    }

    #[bench]
    pub fn bench_chinook_metal(bencher: &mut Bencher) {
        let (strings, chunks) = chinook();
        bencher.iter(|| {
            black_box(chinook_metal(strings.clone(), chunks.clone()));
        });
    }
}