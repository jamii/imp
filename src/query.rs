use chunk::*;

#[derive(Clone, Debug)]
pub enum Action {
    Sort(usize, Vec<usize>),
    Project(usize),
    SemiJoin(usize, usize),
    Join(usize, usize),
    Debug(usize),
}

#[derive(Clone, Debug)]
pub struct Plan {
    pub actions: Vec<Action>,
    pub result: usize,
}

impl Plan {
    pub fn execute(&self, mut chunks: Vec<Chunk>) -> Chunk {
        for action in self.actions.iter() {
            println!("{:?}", chunks.iter().map(|chunk| chunk.data.len()).collect::<Vec<_>>());
            println!("{:?}", action);
            match action {
                &Action::Sort(ix, ref key) => {
                    let chunk = chunks[ix].sort(&key[..]);
                    chunks[ix] = chunk;
                },
                &Action::Project(ix) => {
                    let chunk = chunks[ix].project();
                    chunks[ix] = chunk;
                }
                &Action::SemiJoin(left_ix, right_ix) => {
                    let (left_chunk, right_chunk) = chunks[left_ix].semijoin(&chunks[right_ix]);
                    chunks[left_ix] = left_chunk;
                    chunks[right_ix] = right_chunk;
                },
                &Action::Join(left_ix, right_ix) => {
                    let chunk = chunks[left_ix].join(&chunks[right_ix]);
                    chunks[left_ix] = Chunk::empty();
                    chunks[right_ix] = chunk;
                }
                &Action::Debug(ix) => {
                    println!("{:?}", chunks[ix]);
                }
            }
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
        Chunk{data: data, row_width: row_width, sort_key: vec![]}
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

    fn chinook_metal() -> Vec<String> {
        use query::Action::*;
        let (mut strings, mut chunks) = chinook();
        let metal = "Heavy Metal Classic".to_owned();
        let query = Chunk{ data: vec![hash(&metal), strings.len() as u64], row_width: 2, sort_key: vec![]};
        strings.push(metal);
        chunks.push(query);
        println!("{:?}", chunks[4]);
        println!("{:?}", chunks[5]);
        println!("{:?}", &strings[..100]);
        let plan = Plan{
            actions: vec![
            // semijoin Query and Playlist on Name
            Sort(5, vec![0]),
            Sort(4, vec![1]),
            SemiJoin(5,4),

            // semijoin Playlist and PlaylistTrack on PlaylistId
            Sort(4, vec![0]),
            Sort(3, vec![0]),
            SemiJoin(4,3),

            // semijoin PlaylistTrack and Track on TrackId
            Sort(3, vec![1]),
            Sort(2, vec![0]),
            SemiJoin(3,2),

            // semijoin Track and Album on AlbumId
            Sort(2, vec![3]),
            Sort(1, vec![0]),
            SemiJoin(2,1),

            // join Artist and Album on ArtistId
            Sort(0, vec![0]),
            Sort(1, vec![3]),
            Join(0, 1),
            // project Artist.Name and AlbumId
            Sort(1, vec![1, 2, 3]),
            Project(1),

            // join Artist*Album and Track on AlbumId
            Sort(1, vec![2]),
            Sort(2, vec![3]),
            Join(1, 2),
            // project Artist.Name and TrackId
            Sort(2, vec![0,1,3]),
            Project(2),

            // join Artist*Album*Track and PlaylistTrack on TrackId
            Sort(2, vec![2]),
            Sort(3, vec![1]),
            Join(2, 3),
            // project Artist.Name and PlaylistId
            Sort(3, vec![0,1,3]),
            Project(3),

            // join Artist*Album*Track*PlaylistTrack and Playlist on PlaylistId
            Sort(3, vec![2]),
            Sort(4, vec![0]),
            Debug(3),
            Debug(4),
            Join(3, 4),
            // project Artist.Name and Name
            Sort(4, vec![0,1,4]),
            Project(4),

            // join Artist*Album*Track*PlaylistTrack*Playlist and Query on Name
            Sort(4, vec![2]),
            Sort(5, vec![0]),
            Join(4, 5),
            // project Artist.Name (without hash)
            Sort(5, vec![1]),
            Project(5),

            Debug(5),
            ],

            result: 5
        };
        let results = plan.execute(chunks);
        results.data.iter().map(|ix| strings[*ix as usize].to_owned()).collect()
    }

    #[test]
    fn test_chinook_metal() {
        assert_eq!(
            chinook_metal().iter().collect::<Vec<_>>(),
            vec!["AC/DC", "Accept", "Black Sabbath", "Metallica", "Iron Maiden", "Mot\u{f6}rhead", "M\u{f6}tley Cr\u{fc}e", "Ozzy Osbourne", "Scorpions"]);
    }
}