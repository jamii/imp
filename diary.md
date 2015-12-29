The time has come to do the visa dance again, which means that for the next couple of months I will working on a new side project while I wait for immigration to decide my fate.

I've spent the last 18 months working on [Eve](http://witheve.com/), a project aimed at helping [knowledge workers](https://en.wikipedia.org/wiki/Knowledge_worker) analyse data, automate tasks and move information around. The focus has been on nailing the learning curve so that beginners can treat it like a simple spreadsheet and only gradually introduce features as they are needed. As I leave we're finally approaching a point where I feel optimistic about the shape of things - that the core ideas are mostly nailed down and we actually have something that could be useful to a lot of people.

However, it's not yet useful for *me*. I spend my days building compilers and IDEs, tasks which are not exactly a core focus for Eve. So if Eve is an experiment in 'programming for knowledge workers' then [Imp](https://github.com/jamii/imp) is an experiment in 'programming for people who make things like Eve'. The core ideas are the same but the immediate priorities are very different. To be able to build something like the Eve editor, I have to care about:

* __Performance__. Eve could survive and be useful even if it were never faster than, say, Excel. The Eve IDE, on the other hand, can't afford to miss a frame paint. That means Imp must be not just fast but predictable - the nemesis of the SufficientlySmartCompiler. It might also mean I need a way to split off long-running computations from the UI.

* __Bootstrapping__. It's not possible to use Eve without the IDE, and both the language and IDE are constantly changing and often incomplete. To make a language I can actually use today I need to figure out how to make it useable immediately and progressively enhance the tools later. The upside is that rather than building a polished interface for a general audience, I only need to build a tolerable starting point that I can work with.

* __UI__. I want to reduce the time it takes to try out new ideas for tools. Previous versions of Eve had a UI interface that was capable enough to [bootstrap a simple IDE](http://incidentalcomplexity.com/images/5.png) but required writing raw html and css. A more recent version sported a [UI editor](http://incidentalcomplexity.com/images/mamj-ui.png) that could handle data binding but only allowed static layout. I'm not sure if there is a better approach that I could feasibly finish - perhaps allowing binding to templates made by existing UI tools?

Performance is the only point for which I actually have something resembling a plan, so that's where I'll begin.

# Runtime

Like Eve, Imp is going to be a [Bloom](http://boom.cs.berkeley.edu/)-like language. There are a couple of stateful tables used for inputs and everything else is built out of [views](https://en.wikipedia.org/wiki/View_%28SQL%29) written in a Turing-complete query language. That means that the internals look more like a relational database than a programming language.

The query optimisation problem is almost the opposite of a normal database though - I know all the queries ahead of time but I don't know what the data will look like. I'm also not planning to attempt incremental view maintenance yet so all the views will be recomputed from scratch on every update. This means that I have to consider index creation as part of the cost of each query, rather than a one-off upfront cost.

[Traditional query optimizers](https://en.wikipedia.org/wiki/IBM_System_R) are out of the window then. Instead I'm planning to rely on good data layout, cache-friendly algorithms and some new (and some old but forgotten) breakthroughs on the theory side.

For performance work it's really important to set a goal so that you know when to stop. The closest good comparison I can think of is [SQLite](https://www.sqlite.org/). It's still a database rather than a language and is optimised for [OLTP](https://en.wikipedia.org/wiki/Online_transaction_processing)-style workloads, but it is often used as the main data-structure for complex apps (eg [Fossil](http://fossil-scm.org/index.html/doc/trunk/www/index.wiki)) and can run in-memory. It's a totally rigged competition because Imp-style workloads will break a lot of the assumptions that SQLite is built on, but it gives me something to aim for.

## Joins

Let's start with a really simple problem - joining two tables of 1E6 random 64bit integers each drawn from U(1,1E6). I will count both the time to solve the join and the time taken to build the indexes. I'm just trying to get a rough sense of how expensive various operations are and there is an ocean betwen me and [Zed Shaw](http://zedshaw.com/archive/programmers-need-to-learn-statistics-or-i-will-kill-them-all/) so it's probably safe to just give mean times.

SQLite gives the baseline:

``` sql
SELECT count(A.id) FROM A INNER JOIN B WHERE A.id=B.id;
/* ~1150ms to index A + ~750ms to join */
```

Let's try replicating this in Rust. Sadly [std::collection::BTreeMap](https://doc.rust-lang.org/std/collections/struct.BTreeMap.html) does terribly on this problem regardless of what node size I pick, probably because of the linear search. [std::collection::HashMap](https://doc.rust-lang.org/std/collections/struct.HashMap.html) does a better job, even though it's using a [stronger and slower hash](https://doc.rust-lang.org/std/hash/struct.SipHasher.html) than most databases would bother with.

``` rust
let mut results = Vec::with_capacity(max_size);
let mut index = HashSet::with_capacity(max_size);
for id in ids_a.iter() {
    index.insert(*id);
}
for id in ids_b.iter() {
    if index.contains(id) {
        results.push(*id);
    }
}
black_box(results);
// 107ms to index A + 100ms to join
```

Why is this so much faster than SQLite? OLTP databases tend to have a [ton of overhead](http://nms.csail.mit.edu/~stavros/pubs/OLTP_sigmod08.pdf), most of which is used to support features Imp won't need. For example, even though I am running SQLite in-memory it still has to go through the same interface as is used for on-disk tables. Another reason is that SQLite is designed under the assumption that queries will be small and much more common than index building, so it is optimised for fast lookups rather than bulk joins. Yet another reason why this competition is rigged.

Let's try something even simpler - just sort both tables and then iterate through them in parallel.

``` rust
pub fn intersect_sorted(ids_a: &Vec<Id>, ids_b: &Vec<Id>) -> Vec<Id> {
    let mut results = Vec::with_capacity(max(ids_a.len(), ids_b.len()));
    let mut ix_a = 0;
    let mut ix_b = 0;
    loop {
        match (ids_a.get(ix_a), ids_b.get(ix_b)) {
            (Some(&a), Some(&b)) => {
                match a.cmp(&b) {
                    Ordering::Less => {
                        ix_a += 1;
                    }
                    Ordering::Equal => {
                        let mut end_ix_a = ix_a;
                        while ids_a.get(end_ix_a) == Some(&a) { end_ix_a += 1; }
                        let mut end_ix_b = ix_b;
                        while ids_b.get(end_ix_b) == Some(&b) { end_ix_b += 1; }
                        for ix in (ix_a..end_ix_a) {
                            for _ in (ix_b..end_ix_b) {
                                results.push(ids_a[ix]);
                            }
                        }
                        ix_a = end_ix_a;
                        ix_b = end_ix_b;
                    }
                    Ordering::Greater => {
                        ix_b += 1;
                    }
                }
            }
            _ => break,
        }
    }
    results
}
```

``` rust
// the clone unfairly penalises this test, since in a real use I could just sort in place
let mut sorted_a = ids_a.clone();
let mut sorted_b = ids_b.clone();
sorted_a.sort();
sorted_b.sort();
let results = intersect_sorted(&sorted_a, &sorted_b);
black_box(results);
// 162ms to sort A and B + 20ms to join
```

If I vary the size of the tables the hashing wins on small tables and sorting wins on large tables. That's because my naive use of the HashMap is jumping all over memory for each lookup and on large tables that starts to cause expensive cache misses. There is a [ton](http://dl.acm.org/citation.cfm?id=2732227) [of](http://dl.acm.org/citation.cfm?id=2619232) [research](https://github.com/frankmcsherry/blog/blob/master/posts/2015-08-15.md) on the tradeoffs between sorting and hashing and much more sophisticated implementions exist for each.

For my purposes, the main concern is ease of implementation. I can't use [std::slice::sort](https://doc.rust-lang.org/std/primitive.slice.html#method.sort) because my tables won't be simple Vecs and I can't use [std::collection::HashMap](https://doc.rust-lang.org/std/collections/struct.HashMap.html) because it requires the size of keys to be known at compile time. I'm going to have to roll my own. [Radix sort](https://en.wikipedia.org/wiki/Radix_sort) to the rescue!

``` rust
pub fn radix_sort(ids: &mut Vec<Id>) {
    let ids: &mut Vec<[u8; 8]> = unsafe{ ::std::mem::transmute(ids) };
    let mut buffer = ids.clone();
    let mut counts = [[0; 256]; 8];
    for id in ids.iter() {
        for offset in (0..8) {
            counts[offset][id[offset] as usize] += 1
        }
    }
    let mut buckets = [[0; 256]; 8];
    for offset in (0..8) {
        for ix in (1..256) {
            buckets[offset][ix] = buckets[offset][ix-1] + counts[offset][ix-1];
        }
    }
    for offset in (0..8) {
        for id in ids.iter() {
            let byte = id[offset] as usize;
            buffer[buckets[offset][byte]] = *id;
            buckets[offset][byte] += 1;
        }
        ::std::mem::swap(&mut buffer, ids);
    }
}
```

``` rust
// the clone unfairly penalises this test, since in a real use I could just sort in place
let mut sorted_a = ids_a.clone();
let mut sorted_b = ids_b.clone();
radix_sort(&mut sorted_a);
radix_sort(&mut sorted_b);
let results = intersect_sorted(&sorted_a, &sorted_b);
black_box(results);
// 60ms to sort A and B + 19ms to join
```

Easy to implement and fast too, even with all the bounds checks that I didn't bother taking out. I can handle types with variable lengths (like strings) by sorting their hashes instead - again showing the duality between sorting and hashing.

How much faster could I get? Let's break this down into really simple problems:

``` rust
// sequential read pass - 0.4ms
let mut sum = 0;
for ix in (0..ids.len()) {
    unsafe{ sum += *ids.get_unchecked(ix); }
}
black_box(sum);

// sequential write pass - 1.2ms
for ix in (0..ids.len()) {
    unsafe{ *buffer.get_unchecked_mut(ix) = *ids.get_unchecked(ix); }
}
black_box(&buffer);

// random write pass - 6.8ms
for ix in (0..ids.len()) {
    let id = unsafe{ *ids.get_unchecked(ix) };
    unsafe{ *buffer.get_unchecked_mut(id as usize) = id; }
}
black_box(&buffer);
```

I'm talking nanobenchmarks now and any claim to science has long gone out the window, but this is still a useful sanity check. Radix sort does 1 read pass and 8 write passes and comes to 30ms per table. Even if I removed all the logic, turned off the bounds checks and made the writes totally predictable, it would still cost us (1 * 0.4) + (8 * 1.2) = 10ms just to touch all the memory using these primitives. That means I don't have much to gain from micro-optimising this code - I would have to change the algorithm to do significantly better.

Radix join has some other nice properties. It only makes one memory allocation (for the buffer). The sort time and join time both scale nearly linearly in tests from 1<<10 elements to 1<<29 elements. The time is incredibly consistent across different data-sets (the only outlier I've found being perfect ranges like (0..1<<29) which I suspect may be causing cache collisions because the write addresses are always large powers of two apart). It fits the bill for a simple, predicable runtime.

## Storage

I expect views to be built from scratch each time, rather than incrementally updated, and I am using sorting instead of building data-structures for indexes. That means that I can just store each table as a single chunk of memory, which is ideal in terms of memory locality and reducing pressure on the allocator.

Joining will be common and the most expensive operation is sorting and I expect tables to be highly normalised. Compression and row reconstruction both make sorting harder so a [column store](https://en.wikipedia.org/wiki/Column-oriented_DBMS) is probably a bad idea. Row store it is.

Handling variable-length data like strings is painful in most SQL databases. If it's stored inline in the table the user either has to pick a maximum length or the implementation has to give up fixed-size rows which breaks in-place sorting. Since most of the data in Imp will be from views we can also end up with many references to a given string, which would all be separate copies if they were stored inline. I'll just keep store a hash and a pointer instead and give up on memory locality for string comparisons.

I also want to be able to treat all the data as just plain bytes so that the query engine doesn't have any special cases for eg reference-counting string pointers. Luckily, I can take advantage of the semantics of Bloom to do reference counting out-of-band - I can just count the inserts and removes for input tables and stateful views at the end of each tick.

So that leaves us with these types:

``` rust
type Id = u64;
type Hash = u64;
type Number = f64;
type Text = &'static String;

#[derive(Clone, Debug)]
enum Value {
    Id(Id),
    Number(Number),
    Text(Hash, Text),
}
```

(Note the `&'static String` - this is Rust-speak for "a string will live forever". That will change later on when I start reference counting strings.)

I could store views as a `Vec<Value>` but this works out to be pretty wasteful. An enum always takes enough space to store the largest possible value - in this case a Value::Text(Hash, Text). Thats 1 byte for the enum tag, 8 for the hash and 8 for the text pointer. After alignment we end up with a brutal 24 bytes per value. I almost want to do it anyway just to keep things simple, but good memory locality is pretty much the only trick I have up my sleeve here.

Instead I'm going to enforce that each column has a fixed type and then manage my own data packing. This saves me a ton of memory and will also cut down on dynamic type checks when I add functions later on.

The underlying layer just treats every row as a sequence of bytes and has no idea where each column starts and ends or what type it is.

``` rust
#[derive(Clone, Debug)]
struct Chunk {
    data: Vec<u64>,
    row_width: usize,
}
```

Then the layer on top tracks the mapping between fields and data.

``` rust
#[derive(Copy, Clone, Debug)]
enum Kind {
    Id,
    Number,
    Text,
}

#[derive(Clone, Debug)]
struct Relation {
    fields: Vec<Id>,
    kinds: Vec<Kind>,
    chunk: Chunk,
}
```

I'm not sure yet what I want the external interface for Relation to look like, so I'll move on to the internals instead.

## Operators

We need a whole army of relational operations.

``` rust
impl Chunk {
    fn sort(&self, key: &[usize]) -> Chunk
    fn groups<'a>(&'a self) -> Groups<'a>
    fn project(&self) -> Chunk
    fn diffs<'a>(&'a self, other: &'a Chunk) -> Diffs<'a>
    fn semijoin(&self, other: &Chunk) -> (Chunk, Chunk)
    fn join(&self, other: &Chunk) -> Chunk
    fn union(&self, other: &Chunk) -> Chunk
    fn difference(&self, other: &Chunk) -> Chunk
}
```

All of them are going to rely on their inputs being sorted in the correct order. Rather than passing in a key for each operation I'm instead storing the sort key in the chunk and using that for any subsequent operations, so you would write eg `a.sort(&[0, 1]).join(b.sort(&[3, 2]))` for the query `where a.0=b.3 and a.1=b.2`.

There are two iterators: `groups` yields slices of consecutive rows which are equal on the sort key and `diffs` runs through two sorted chunks and matches up groups which are equal on the corresponding sort keys.

``` rust
#[derive(Clone, Debug)]
pub struct Groups<'a> {
    pub chunk: &'a Chunk,
    pub ix: usize,
}

impl<'a> Iterator for Groups<'a> {
    type Item = &'a [u64];

    fn next(&mut self) -> Option<&'a [u64]> {
        let data = &self.chunk.data;
        let row_width = self.chunk.row_width;
        let key = &self.chunk.sort_key[..];
        if self.ix >= data.len() {
            None
        } else {
            let start = self.ix;
            let mut end = start;
            loop {
                end += row_width;
                if end >= data.len()
                || compare_by_key(&data[start..start+row_width], key, &data[end..end+row_width], key) != Ordering::Equal {
                    break;
                }
            }
            self.ix = end;
            Some(&data[start..end])
        }
    }
}

#[derive(Clone, Debug)]
pub struct Diffs<'a> {
    pub left_key: &'a [usize],
    pub left_groups: Groups<'a>,
    pub left_group: Option<&'a [u64]>,
    pub right_key: &'a [usize],
    pub right_groups: Groups<'a>,
    pub right_group: Option<&'a [u64]>,
}

impl<'a> Iterator for Diffs<'a> {
    type Item = Diff<'a>;

    fn next(&mut self) -> Option<Diff<'a>> {
        match (self.left_group, self.right_group) {
            (Some(left_words), Some(right_words)) => {
                match compare_by_key(left_words, self.left_key, right_words, self.right_key) {
                    Ordering::Less => {
                        let result = Some(Diff::Left(left_words));
                        self.left_group = self.left_groups.next();
                        result
                    }
                    Ordering::Equal => {
                        let result = Some(Diff::Both(left_words, right_words));
                        self.left_group = self.left_groups.next();
                        self.right_group = self.right_groups.next();
                        result
                    }
                    Ordering::Greater => {
                        let result = Some(Diff::Right(right_words));
                        self.right_group = self.right_groups.next();
                        result
                    }
                }
            }
            _ => None,
        }
    }
}
```

All of the relational operators are then pretty straightforward eg:

``` rust
fn join(&self, other: &Chunk) -> Chunk {
    let mut data = vec![];
    for diff in self.diffs(other) {
        match diff {
            Diff::Both(self_words, other_words) => {
                for self_row in self_words.chunks(self.row_width) {
                    for other_row in other_words.chunks(other.row_width) {
                        data.extend(self_row);
                        data.extend(other_row);
                    }
                }
            }
            _ => ()
        }
    }
    let row_width = self.row_width + other.row_width;
    let mut sort_key = self.sort_key.clone();
    for word_ix in other.sort_key.iter() {
        sort_key.push(self.row_width + word_ix);
    }
    Chunk{data: data, row_width: row_width, sort_key: sort_key}
}
```

We can compare the performance to our previous tests to see how much overhead has been added:

``` rust
let mut chunk_a = Chunk{ data: ids_a.clone(), row_width: 1, sort_key: vec![] };
let mut chunk_b = Chunk{ data: ids_b.clone(), row_width: 1, sort_key: vec![] };
chunk_a = chunk_a.sort(&[0]);
chunk_b = chunk_b.sort(&[0]);
black_box(chunk_a.join(&chunk_b));
// 84ms to sort A and B + 46ms to join
```

The sort time has gone up by 30%, which is bearable, but the join time has more than doubled. I originally wrote the join directly before pulling out the nice iterators, so I know that those didn't affect the performance. I've tried cutting parts out, removing abstractions, disabling bounds checks etc with no significant effect. As best as I can tell, the culprit is:

``` rust
pub fn compare_by_key(left_words: &[u64], left_key: &[usize], right_words: &[u64], right_key: &[usize]) -> Ordering {
    for ix in 0..min(left_key.len(), right_key.len()) {
        match left_words[left_key[ix]].cmp(&right_words[right_key[ix]]) {
            Ordering::Less => return Ordering::Less,
            Ordering::Equal => (),
            Ordering::Greater => return Ordering::Greater,
        }
    }
    return Ordering::Equal;
}
```

Where we used to have a single comparison, we now have a bunch of array reads and branches bloating up the inner join loop, even though in this particular benchmark the actual effect is exactly the same. This is a good example of why code generation is such a big deal in database research at the moment - you can get huge improvements from specialising functions like this to the exact data layout and parameters being used. I would love to have something like [Terra](http://terralang.org/) or [LMS](http://scala-lms.github.io/) with the same level of polish and community support as Rust.

## Plans

The compiler is going to output query plans, which in Imp are just a list of actions to run.

``` rust
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
```

The query engine just directly interprets these plans.

``` rust
impl Plan {
    pub fn execute(&self, mut chunks: Vec<Chunk>) -> Chunk {
        for action in self.actions.iter() {
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
```

I wrote some quick and hacky csv import code so I can play with the [Chinook dataset](http://chinookdatabase.codeplex.com/).

```
sqlite> SELECT count(*) FROM Artist;
275
sqlite> SELECT count(*) FROM Album;
347
sqlite> SELECT count(*) FROM Track;
3503
sqlite> SELECT count(*) FROM PlaylistTrack;
8715
sqlite> SELECT count(*) FROM Playlist;
18
```

Let's level the playing field somewhat and use a nice OLTP-style query - finding all the artists on the "Heavy Metal Classic" playlist. The Chinook db comes with prebuilt indexes and this query only touches a small subset of the data - exactly the use case sqlite is intended for.

``` python
In [9]: def test():
    for _ in range(0,10000):
        cur.execute('SELECT DISTINCT Artist.Name FROM Playlist JOIN PlaylistTrack ON Playlist.PlaylistId=PlaylistTrack.PlaylistId JOIN Track ON PlaylistTrack.TrackId=Track.TrackId JOIN Album ON Track.AlbumId=Album.AlbumId JOIN Artist ON Album.ArtistId = Artist.ArtistId WHERE Playlist.Name="Heavy Metal Classic"')
        cur.fetchall()
   ...:

In [10]: time test()
CPU times: user 12.6 s, sys: 48.1 ms, total: 12.7 s
Wall time: 12.7 s

In [11]: time test()
CPU times: user 12.7 s, sys: 48 ms, total: 12.7 s
Wall time: 12.7 s

```

So thats 1.27 ms per query for sqlite. I hand-compiled the same query into an Imp plan:

``` rust
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
    ],

    result: 5
};
```

I'll go into more detail on where this plan came from when we get to the compiler section, but an important note is that the planning algorithm doesn't use any information about the actual data, just the query and the schema.

```
test query::tests::bench_chinook_metal     ... bench:   1,976,232 ns/iter (+/- 43,442)
```

So close! And without any indexes or statistics. The urge to optimise is huge, but I'm going to focus and get the compiler working first.

## Planning

[OLTP](https://en.wikipedia.org/wiki/Online_transaction_processing) databases are built for small, frequent changes to data and join-heavy queries which touch a small part of the dataset. They typically rely on indexes and use infrequently gathered statistics about the data to estimate how fast various query plans will run. When these statistics are incorrect, old or just [misused]() the query planner can make disastrously bad decisions.

[OLAP](https://en.wikipedia.org/wiki/Online_analytical_processing) databases are built for infrequently changing data and aggregation-heavy queries which touch a large part of the dataset. They typically rely on compact data layout and compression rather than indexes. They also on gathered statistics but, in my experience, are less sensitive to mistakes because most queries already involve table scans and aggregation is less susceptible than joining to blowing up and producing huge intermediate results.

Imp doesn't fit nicely into either of these categories. Inputs and views may change entirely on each execution. Queries might be join-heavy or aggregation-heavy and might touch any amount of data. On the other hand, a typical OLTP application might issue the same query hundreds of times with different parameters (eg get the friend count for user X) where an Imp program would build a single view over all the parameters (eg get the friend count for all active users). I also want to have predictable and stable performance so I can build interactive programs without suffering mysterious and unpredictable pauses, so I would prefer to have a planner that always produces mediocre plans over one that mostly produces amazing plans but occasionally explodes.

I've started with Yannakakis' algorithm, which provides tight guarantees for a large class of joins and does not require indexes or statistics. Unfortunately the original paper doesn't seem to be openly available, but the lecture notes [here](http://infolab.stanford.edu/~ullman/cs345notes/slides01-3.pdf) and [here](http://infolab.stanford.edu/~ullman/cs345notes/slides01-5.pdf) give a good overview and I'll try to explain it informally here too.

Let's start with a simple query:

``` sql
-- Find all companies with employees who are banned
SELECT * FROM
Companies
JOIN Users WHERE User.Employer = Company.Id
JOIN Logins WHERE Logins.UserId = Users.Id
JOIN Bans WHERE Bans.IP = Logins.IP
```

There are no filters or constants and we are selecting everything. We don't have indexes so we have to read all of the inputs to the query. We also have to produce all the outputs, obviously. So that gives us a lower bound on the runtime of O(IN + OUT).

Suppose we just walked through the input tables one by one and joined them together. What could go wrong? Imagine we have 1,000,000 companies and they each have one 10,000 employees, so joining Companies with Users would produce 10,000,000,000 rows. We then have to join each of those rows with, say, 100 logins per user. But we might not have banned anyone at all, so the final result is empty and we have done a ton of unnecessary work.

The core problem here is that if we naively join tables together we may end up with intermediate results that are much larger than the final result. How much extra work this causes depends on what order the tables are joined in and how the data is distributed, both of which are hard to predict when writing the query. Traditional query planners try to estimate the size of intermediate results based on the statistics they gather about the currrent dataset.

Yannakakis algorithm is much simpler. It works like this:

1. If there is only one table, you are finished!
2. Otherwise, pick an __ear__ table and it's __parent__ table where all the joined columns in the ear are joined on the parent
3. [Semijoin](https://en.wikipedia.org/wiki/Relational_algebra#Semijoin_.28.E2.8B.89.29.28.E2.8B.8A.29) the ear with it's parent ie remove all the rows in each table that do not join with any row in the other table
4. Recursively solve the rest of the query without the ear table
5. Join the results with the ear table

The crucial part is step 2 which removes any rows that do not contribute to the output (the proof of this is in the notes linked earlier). This guarantees that results at step 3 contain at most IN + OUT rows. Each recursion step removes one table, so the whole algorithm runs O(IN + OUT) time which is the best we can do in this situation. It's also simple to implement and easy to predict.

Our example query is a little too simplistic though. Most realistic queries only want a few columns:

``` sql
-- Find all companies with employees who are banned
SELECT DISTINCT(Companies.Id) FROM
Companies
JOIN Users WHERE User.Employer = Company.Id
JOIN Logins WHERE Logins.UserId = Users.Id
JOIN Bans WHERE Bans.IP = Logins.IP
```

Let's modify the algorithm to handle this:

1. If there is only one table, remove all unwanted columns (ie those that are not needed for the output or for later joins)
2. Otherwise, pick an __ear__ table and it's __parent__ table where all the joined columns in the ear are joined on the parent
3. [Semijoin](https://en.wikipedia.org/wiki/Relational_algebra#Semijoin_.28.E2.8B.89.29.28.E2.8B.8A.29) the ear with it's parent ie remove all the rows in each table that do not join with any row in the other table
4. Recursively solve the rest of the query without the ear table
5. Join the results with the ear table and remove all unwanted columns (ie those that are not needed for the output or for later joins)

This messes with our runtime guarantees. Even if we only return one company for the above query they might have 1000 banned employees. To predict the runtime cost we now have to think about how many redundant results we get in the output. It still works out pretty well though.

There is a much worse problem - step 2 might fail. We can't always find an ear table. An example of a query with no ears is:

``` sql
-- Count triangles in graph
SELECT COUNT(*) FROM
Edges AS A
JOIN Edges AS B WHERE A.To = B.From
JOIN Edges AS C WHERE B.To = C.From AND C.To = A.From
```

A is joined on both A.From and A.To, but B only covers A.To and C only covers A.From so A is not an ear. Similarly for B and C.

When working with cylic queries like this it's really hard to prevent large intermediate results. For example, it is possible to [construct a dataset](http://arxiv.org/abs/1310.3314) for the above query where joining any two tables produces O(n^2) results but the whole query only produces O(n) results. Traditional query optimisers can't handle this well and only in the last few years has there been any progress on general purpose algorithms for cyclic queries. I'm hoping to bring in some of that work later but for now I'll just ban cyclic queries entirely.

The code that implements the query planner in Imp is mostly plumbing. There is a data-structure that tracks the current state of the plan:

``` rust
#[derive(Clone, Debug)]
pub struct State {
    pub actions: Vec<runtime::Action>,
    pub chunks: Vec<Chunk>,
    pub to_join: Vec<usize>,
    pub to_select: Vec<VariableId>,
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub kinds: Vec<Kind>,
    pub bindings: Vec<Option<VariableId>>,
}
```

A few helper functions:

``` rust
impl Chunk {
    fn vars(&self) -> HashSet<VariableId>
    fn project_key(&self, vars: &Vec<VariableId>) -> (Vec<usize>, Vec<Kind>, Vec<Option<VariableId>>)
    fn sort_key(&self, vars: &Vec<VariableId>) -> Vec<usize>
}

impl State {
    pub fn project(&mut self, chunk_ix: usize, sort_vars: &Vec<VariableId>, select_vars: &Vec<VariableId>)
    pub fn semijoin(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>)
    pub fn join(&mut self, left_chunk_ix: usize, right_chunk_ix: usize, vars: &Vec<VariableId>)
```

And finally the core planner:

``` rust
impl State {
    pub fn find_ear(&self) -> (usize, usize) {
        for &chunk_ix in self.to_join.iter() {
            let chunk = &self.chunks[chunk_ix];
            let vars = chunk.vars();
            let mut joined_vars = HashSet::new();
            for &other_chunk_ix in self.to_join.iter() {
                if chunk_ix != other_chunk_ix {
                    let other_vars = self.chunks[other_chunk_ix].vars();
                    joined_vars.extend(vars.intersection(&other_vars).cloned());
                }
            }
            for &other_chunk_ix in self.to_join.iter() {
                if chunk_ix != other_chunk_ix {
                    let other_vars = self.chunks[other_chunk_ix].vars();
                    if joined_vars.is_subset(&other_vars) {
                        return (chunk_ix, other_chunk_ix);
                    }
                }
            }
        }
        panic!("Cant find an ear in:\n {:#?}", self);
    }

    pub fn compile(&mut self) -> usize {
        let to_select = self.to_select.clone();
        if self.to_join.len() == 2 {
            let left_ix = self.to_join[0];
            let right_ix = self.to_join[1];
            let left_vars = self.chunks[left_ix].vars();
            let right_vars = self.chunks[right_ix].vars();
            let join_vars = left_vars.intersection(&right_vars).cloned().collect();
            self.project(left_ix, &join_vars, &to_select);
            self.project(right_ix, &join_vars, &to_select);
            self.join(left_ix, right_ix, &join_vars);
            self.project(right_ix, &vec![], &to_select);
            right_ix
        } else {
            let (ear_ix, parent_ix) = self.find_ear();
            let ear_vars = self.chunks[ear_ix].vars();
            let parent_vars = self.chunks[parent_ix].vars();
            let join_vars = ear_vars.intersection(&parent_vars).cloned().collect();
            self.project(ear_ix, &join_vars, &to_select);
            self.project(parent_ix, &join_vars, &parent_vars.iter().cloned().collect());
            self.semijoin(ear_ix, parent_ix, &join_vars);
            self.to_join.retain(|&ix| ix != ear_ix);
            self.to_select = ordered_union(&join_vars, &to_select);
            let result_ix = self.compile();
            self.join(ear_ix, result_ix, &join_vars);
            self.project(result_ix, &vec![], &to_select);
            result_ix
        }
    }
}
```

Note the panic on not finding an ear. Also, note the base case in the planner is for two tables, saving an unnecessary semijoin.

Finally, the whole process is kicked off from:

``` rust
impl Query {
    pub fn compile(&self, program: &Program) -> runtime::Query {
        let upstream = self.clauses.iter().map(|clause| {
            let ix = program.ids.iter().position(|id| *id == clause.view).unwrap();
            program.schedule[ix]
        }).collect();
        let mut state = State{
            actions: vec![],
            chunks: self.clauses.iter().map(|clause| {
                let ix = program.ids.iter().position(|id| *id == clause.view).unwrap();
                Chunk{
                    kinds: program.schemas[ix].clone(),
                    bindings: clause.bindings.clone(),
                }
            }).collect(),
            to_join: (0..self.clauses.len()).collect(),
            to_select: self.select.clone(),
        };
        let result = state.compile();
        runtime::Query{upstream: upstream, actions: state.actions, result: result}
    }
}
```

I've already written the dataflow compiler and a crude parser, so let's look at those quickly before seeing some benchmark numbers.

## Flow

The main job of the rest of the Imp runtime is to keep all the views up to date as the inputs change. The state of the runtime is tracked in:

``` rust
#[derive(Clone, Debug)]
pub struct Program {
    pub ids: Vec<ViewId>,
    // TODO store field ids too
    pub schemas: Vec<Vec<Kind>>,
    pub states: Vec<Rc<Chunk>>,
    pub views: Vec<View>,
    pub downstreams: Vec<Vec<usize>>,
    pub dirty: Vec<bool>, // should be BitSet but that has been removed from std :(

    pub strings: Vec<String>, // to be replaced by gc
}

#[derive(Clone, Debug)]
pub enum View {
    Input,
    Query(Query),
}

#[derive(Clone, Debug)]
pub struct Query {
    pub upstream: Vec<usize>,
    pub actions: Vec<Action>,
    pub result: usize,
}
```

The views are all stored in some order decided by the compiler. The upstream and downstream fields track the positions of dependencies between the views. Whenever we change an input we have to dirty all the downstream views.

``` rust
impl Program {
    pub fn set_state(&mut self, id: ViewId, state: Chunk) {
        let &mut Program{ref ids, ref mut states, ref views, ref downstreams, ref mut dirty, ..} = self;
        let ix = ids.iter().position(|&other_id| other_id == id).unwrap();
        match views[ix] {
            View::Input => {
                states[ix] = Rc::new(state);
                for &downstream_ix in downstreams[ix].iter() {
                    dirty[downstream_ix] = true;
                }
            }
            View::Query(_) => {
                panic!("Can't set view {:?} - it's a query!", id);
            }
        }
    }
}
```

Then to run the program we just keep updating views until nothing is dirty.

``` rust
impl Program {
    pub fn run(&mut self) {
        let &mut Program{ref mut states, ref views, ref downstreams, ref mut dirty, ref strings, ..} = self;
        while let Some(ix) = dirty.iter().position(|&is_dirty| is_dirty) {
            match views[ix] {
                View::Input => panic!("How did an input get dirtied?"),
                View::Query(ref query) => {
                    dirty[ix] = false;
                    let new_chunk = query.run(strings, &states[..]);
                    if *states[ix] != new_chunk {
                        states[ix] = Rc::new(new_chunk);
                        for &downstream_ix in downstreams[ix].iter() {
                            dirty[downstream_ix] = true;
                        }
                    }
                }
            }
        }
    }
}
```

If there are cycles in the graph of views then the order in which they are run could potentially affect the result. I'll cover that problem in more detail when I get to implementing [stratification](https://en.wikipedia.org/wiki/Stratification_%28mathematics%29).

## Syntax

My parser is a thing of shame so let's just talk about the syntax itself. I'm aiming for the madlib style that was used in [earlier Eve demos](http://incidentalcomplexity.com/images/5.png). I like this style because the table names become self-documenting and because it (mildly) encourages normalization and writing [facts rather than state](https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/ValueOfValues.md).

Here is a snippet of Imp:

```
artist ?a:id is named ?an:text
= data/Artist.csv 0 1

?an:text is on a metal playlist
+
playlist ?p is named "Heavy Metal Classic"
track ?t is on playlist ?p
track ?t is on album ?al
album ?al is by artist ?a
artist ?a is named ?an
```

There are two views here. The first defines an input view called `artist _ is named _` and loads data from Artist.csv, parsing ids from column 0 and text from 1. The second defines are query view called `_ is on a metal playlist`. The body of the query joins pulls data from five other views, joining them wherever the same variable is used in more than one place.

I'm deliberately using short variables to keep the focus on the rest of the sentence and make it easier to quickly scan for joins.

The reason that queries are indicated by a `+` is that I want to introduce non-monotonic reasoning later on so I can write queries like:

```
?an:id can fly
+
?an is a bird
-
?an is a penguin
+
?an is Harry the Rocket Penguin
```

This reads as "birds can fly, but penguins can't, but Harry the Rocket Penguin can". This sort of reasoning is clumsy in traditional datalog and it often comes up when setting defaults or when updating values (all the values, minus the one I'm changing, plus it's new value).

Functions can be treated as infinite relations:

```
?a + ?b = ?c
```

There are some simple static checks we can use to ensure that the resulting query doesn't produce infinite results. More on that when I actually implement functions.

I haven't decided how I want to handle aggregation yet, so there is no syntax for it.

## First steps

I'm excited to show Imp's first whole program:

```
playlist ?p:id is named ?pn:text
= data/Playlist.csv 0 1

track ?t:id is on playlist ?p:id
= data/PlaylistTrack.csv 1 0

track ?t:id is on album ?al:id
= data/Track.csv 0 2

album ?al:id is by artist ?a:id
= data/Album.csv 0 2

artist ?a:id is named ?an:text
= data/Artist.csv 0 1

?pn:text is the name of a metal playlist
= data/Metal.csv 0

?an:text is on a metal playlist
+
?pn is the name of a metal playlist
playlist ?p is named ?pn
track ?t is on playlist ?p
track ?t is on album ?al
album ?al is by artist ?a
artist ?a is named ?an
```

This is the same example query I've been using all along but now it's running through the whole compiler. I haven't implemented constants yet so I'm loading the playlist name from Metal.csv instead.

So how does it stack up against sqlite?

```
let bootstrap_program = Program::load(&["data/chinook.imp", "data/metal.imp"]);
let runtime_program = bootstrap_program.compile();
bencher.iter(|| {
    let mut runtime_program = runtime_program.clone();
    runtime_program.run();
    black_box(&runtime_program.states[6]);
});
// test bootstrap::tests::bench_metal_run       ... bench:     864,801 ns/iter (+/- 45,810)
```

A beautiful 0.86 ms vs SQLites 1.2ms. I'm gaining some advantage from normalizing the database and I got lucky with the clause ordering and it's not much of a benchmark to begin with, but I'm still feeling pretty good :)

What was that about the clause ordering? The planner picks the first ear it can find and it searches the clauses in the order they are given in the program. If we reverse the ordering we get a plan that takes 1.7ms, because it runs the filtering phase from artist to playlist instead of the other direction, resulting in absolutely no filtering. No matter what order is chosen, every plan has to sort all of the inputs once and then all of the intermediate results once. The size of the intermediate results are still bounded, so we can expect the difference between the best and worst plans to at most 2x.

## Filtering

I added a filtering phase at the start of each query to handle self-joins and constants, so we can now write queries like:

```
?x is friends with #42
?x has 7 friends
?x is from "England"
?x is friends with ?x
```

Whenever a self-join or a constant is spotted, the planner adds an action that calls one of:

``` rust
impl Chunk {
    pub fn selfjoin(&self, left_ix: usize, right_ix: usize) -> Chunk {
        let mut data = vec![];
        for row in self.data.chunks(self.row_width) {
            if row[left_ix] == row[right_ix] {
                data.extend(row);
            }
        }
        Chunk{ data: data, row_width: self.row_width}
    }

    pub fn filter(&self, ix: usize, value: u64) -> Chunk {
        let mut data = vec![];
        for row in self.data.chunks(self.row_width) {
            if row[ix] == value {
                data.extend(row);
            }
        }
        Chunk{ data: data, row_width: self.row_width}
    }
}
```

## Data entry

Now that the parser can handle constants, I can add tables of constants:

```
there is an edge from ?a:id to ?b:id
=
#0 #1
#1 #2
#2 #3
#3 #4
#3 #1
```

## Unions

Finally, the reason for the weird "+" syntax. Queries can be made up of multiple stages which can each add or remove results:

```
?a:text can fly
+
?a is a bird
-
?a is a penguin
+
?a is Harry the Rocket Penguin
```

This starts to enable the use of recursive views too, so we can write:

```
there is a path from ?a:id to ?b:id
+
there is an edge from ?a to ?b
+
there is an edge from ?a to ?c
there is a path from ?c to ?b
```

Most of the work was in the monstrous parser. The final runtime structure is very simple:

``` rust
#[derive(Debug, Clone)]
pub struct Union {
    pub upstream: Vec<usize>,
    pub members: Vec<Member>,
    pub key: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Member {
    Insert,
    Remove,
}

impl Union {
    fn run(&self, states: &[Rc<Chunk>]) -> Chunk {
        assert_eq!(self.members[0], Member::Insert);
        let mut result = Cow::Borrowed(&*states[self.upstream[0]]);
        for ix in 1..self.upstream.len() {
            result = Cow::Owned(match self.members[ix] {
                Member::Insert => result.union(&*states[self.upstream[ix]], &self.key[..]),
                Member::Remove => result.difference(&*states[self.upstream[ix]], &self.key[..]),
            });
        }
        result.into_owned()
    }
}
```

## Interlude

I've been trying to extend Yannakakis' algorithm to handle primitives (ie infinite relations such as addition). I initially thought I could just treat them as normal relations during planning and then replace them by finite relations during the semijoin phase. The trouble is that many uses of primitives create cyclic queries, which I was hoping to punt on for a while longer eg

```
?bid is the max buy in market ?m with price ?bp and quant ?bq
?sid is the min sell in market ?m with price ?sp and quant ?sq
?bp >= ?sp
?q = min(?bq, ?sq)
```

In a query like this you really don't want to replace >= or min by finite relations until after joining the buys and sells on ?m, otherwise you end up with O(n**2) rows in the replacements. In general it looks like primitives shouldn't be applied until all their inputs have been joined together, but that means that they can't take part in the semijoin phase which breaks the guarantees of Yannakakis' algorithm.

An alternative is to ignore primitives while planning the join tree and then insert them as extra nodes afterwards. This can blow up in queries like:

```
person ?p has first name ?fn
person ?p has last name ?ln
?n = concat(?fn, ?ln)
letter ?l is addressed to ?n
```

Ignoring the concat, *a* valid join tree for Yannakakis would be "letter ?l is addressed to ?n" -> "person ?p has first name ?fn" -> "person ?p has last name ?ln". The concat can't be applied until after the last join so the first join is an expensive and unnecessary product.

Intuitively though, it seems that for every query there should be *some* sensible join tree. I'm currently trying to figure out if there is a way to bound the costs of a given tree containing primitives. Then the compiler could just generate every valid tree and choose the tree with the lowest bounds.

Today I'm rereading http://arxiv.org/pdf/1508.07532.pdf and http://arxiv.org/pdf/1508.01239.pdf, both of which calculate bounds for related problems. Hopefully something in there will inspire me. I've been stalled on this for a week or so, so I would be happy to find a crude solution for now and come back to it later.

## Primitives

I finally settled on a solution. I'm pretty sure there are cases where it will do something daft but I'll worry about those when I come to them.

We start by ignoring the primitives, building a join tree just for the views and running a full [GYO reduction](http://infolab.stanford.edu/~ullman/cs345notes/slides01-3.pdf).

``` rust
let mut join_tree = build_join_tree(&chunks);
for chunk_ix in 0..chunks.len() {
    filter(&mut chunks, &mut actions, chunk_ix);
    selfjoin(&mut chunks, &mut actions, chunk_ix);
}
for edge in join_tree.iter().rev() {
    if let &(child_ix, Some(parent_ix)) = edge {
        semijoin(&mut chunks, &mut actions, child_ix, parent_ix);
    }
}
for edge in join_tree.iter() {
    if let &(child_ix, Some(parent_ix)) = edge {
        semijoin(&mut chunks, &mut actions, parent_ix, child_ix);
    }
}
```

Then we repeatedly:

* find the smallest subtree which contains enough variables to apply some primitive
* join together all the chunks in the subtree
* apply the primitive

``` rust
while primitives.len() > 0 {
    let (primitive_ix, subtree) = cheapest_primitive_subtree(&chunks, &join_tree, &primitives);
    let root_ix = collapse_subtree(&mut chunks, &mut actions, &mut join_tree, &subtree);
    apply(&mut chunks, &mut actions, strings, root_ix, &primitives[primitive_ix]);
    primitives.remove(primitive_ix);
}
```

Finally, we join together any remaining chunks and project out the result variables.

``` rust
let remaining_tree = join_tree.clone();
let root_ix = collapse_subtree(&mut chunks, &mut actions, &mut join_tree, &remaining_tree);
sort_and_project(&mut chunks, &mut actions, root_ix, &query.select, &vec![]);
```

The reasoning here is that primitives are usually cheap to compute and may be useful in later joins, so we want to apply them as early as possible. Running the full reduction (instead of the half reduction I was using before) allows joining the chunks in any order, at the cost of some extra sorting. The end result is:

* views which don't use primitives still gain the runtime bounds from Yannakakis
* views which do use primitives have the runtime bounds that Yannakakis would have IF all the primtives were removed from the query

Note that removing primitives can potentially vastly increase the output size, which means that these bounds are much looser. For example:

```
person ?p has first name ?fn
person ?p has last name ?ln
?n = concat(?fn, ?ln)
letter ?l is addressed to ?n
```

```
person ?p has first name ?fn
person ?p has last name ?ln
letter ?l is addressed to ?n
```

Thie first query is guaranteed to take no more time than the second query, which generates every possible combination of letter and person. That's not a very tight bound.

In practice, simple examples like this end up with sensible plans, but in complex queries with multiple primitives it is possible to coerce the compiler into bad decisions. My intuition is that is should be possible to prevent this by being more careful about which order primitives are applied in - choosing the subtree with the smallest bound rather than the least number of nodes - but computing the bounds is complicated and I want to move on to other subjects.

## Notes on design

The rest of the compiler is mostly dull book-keeping but I want to call attention to the style of programming. Over the last year or two I've leaned more and more towards data-oriented design as advocated by eg [Mike Acton](http://www.slideshare.net/cellperformance/data-oriented-design-and-c). The primary reason for that is *not* performance but because I find it prevents me agonising over code organisation and because it plays well with the Rust borrow checker. An example of this is the join tree. A traditional approach would be something like:

``` rust
struct Tree {
    chunk: Chunk,
    children: Vec<Tree>,
}
```

Since everything is connected by pointers I have to think carefully about where to keep data eg if later I am walking the tree and I need a list of the bindings for the chunk, I either have to include the bindings in the Chunk struct beforehand or I have to look it up in some chunk-to-bindings hashtable. Is the chunk hashable? Am I ever going to mutate it?

In Rust I have to think about ownership too. Does the chunk-to-bindings hashtable have it's own copy of the chunk or is it sharing with the tree? The former adds unnecessary copies but the latter imposes a bunch of lifetime annotations that clog up all my code.

A much simpler approach is to store all the information separately and tie it together with a simple key. In this case, I just store the chunks in one vector, the bindings in another vector and use the position in the vector as the key.

``` rust
struct Tree {
    chunk: usize,
    children: Vec<Tree>,
}
```

But we still have a recursive, mutable type which is [painful](http://stackoverflow.com/questions/28608823/how-to-model-complex-recursive-data-structures-graphs) in Rust. Even in a normal language we have to write extra code to handle operations like inserting edges or traversing the tree. Life is easier with a simpler representation.

``` rust
// (child, parent) sorted from root downwards
type Tree = Vec<(usize, Option<usize>)>;
```

Most of the code that touches this the tree becomes delightfully simple eg:

``` rust
while unused.len() > 1 { // one chunk will be left behind as the root
    let (child_ix, parent_ix) = find_join_ear(chunks, &unused);
    unused.retain(|ix| *ix != child_ix);
    tree.push((child_ix, Some(parent_ix)));
}
tree.push((unused[0], None));
tree.rev();
```

There are incidental performance benefits - we now have a single contiguous allocation for the whole tree - but the main benefit is just simplicity. I'm leaning more and more towards just [putting things in arrays](https://youtu.be/JjDsP5n2kSM?t=752) until profiling demands otherwise.

I think it's interesting that the borrow checker directly encourages what I judge to be good design. I wonder what kind of effect that will have on the long-term quality of the Rust ecosystem.

## Aggregates

Aggregates have been a constant ergonomic nightmare in Eve. This shouldn't be surprising - they are the dumping ground for everything non-relational and non-monotonic - all the awkward bits of logic that actually intrinsically require waiting or ordering. They also interact weirdly with set semantics, because projecting out unused columns also removes duplicates in the remaining data which can change the result of an aggregate like `sum`.

So I'm surprised to find myself not entirely hating the design in Imp. There are some places in the compiler that I suspect might be buggy, but I think the semantics at least are sound. Aggregates look like this:

```
company ?c:text spends ?t:number USD
+
person ?p works at company ?c for ?d USD
?t = sum(?d) over ?p

total salary is ?t:number USD
+
person ?p works at company ?c for ?d USD
?t = sum(?d) over ?p ?c
```

Syntactically, aggregates behave exactly like primitives except that there is an optional 'over' clause that controls grouping and sorting. When `sum(?d) over ?p` is applied, it groups the current chunk by everything except ?d and ?p and then sums over ?d in each group, giving the total salary across all people at the same company. Similarly, `sum(?d) over ?p ?c` groups the current chunk by everything except ?d, ?p and ?c, giving the total salary overall.

A weakness of this scheme is it doesn't always capture intent. For example, we might want to change the second query to:

```
total salary at european companies is ?t:number USD
+
person ?p works at company ?c for ?d USD
?t = sum(?d) over ?p ?c
company ?c is based in ?country
?country is in europe
```

This now calculates the total salary per country, not for the whole of Europe. The correct query is:

```
total salary at european companies is ?t:number USD
+
person ?p works at company ?c for ?d USD
?t = sum(?d) over ?p ?c ?country
company ?c is based in ?country
?country is in europe
```

But this can still double-count if it is possible for a company to be based in multiple countries. In cases like this, it may be safer to just split it into two views:

```
person ?p:text works at european company ?c:text for ?d:number USD
+
person ?p works at company ?c for ?d USD
company ?c is based in ?country
?country is in europe

total salary at european companies is ?t:number USD
+
person ?p works at european company ?c for ?d USD
?t = sum(?d) over ?p ?c
```

The upside of specifying groups this way is that can handle sorting too. The primitive `row _` sorts each group in the order specified by 'over' and then numbers them in ascending order. This gives us min, max, limit, pagination etc.

```
?p:text is paid least at their company
+
?p works at ?c for ?d USD
row 1 over ?d ?p

?p:text is in the top ten at their company
+
?p works at ?c for ?d USD
row ?n over -?d ?p
n <= 10
```

The `-?d` in the second example specifies that `?d` should be sorted in descending order.

The implementation of aggregates took very little work since it piggybacks on primitives. The scheduler now allows primitives to specify variables on which they depend non-monotonically and will only schedule the primitive when anything that might filter down those variables has already been applied. In the last example above, if we added the clause `?p is a real employee` it would have to be joined with `?p works at ?c for ?d USD` *before* the rows were sorted and numbered.

The sorting also has to be handled specially. The radix sort used for joining just sorts values by their bitwise representation, which gives the wrong results for numbers and strings. For aggregates I added a hideously inefficient sort function that piggybacks on the stdlib sort.

``` rust
// TODO this is grossly inefficient compared to untyped sort
fn typed_sort(chunk: &Chunk, ixes: &[(usize, Kind, Direction)], strings: &Vec<String>) -> Chunk {
    let mut data = chunk.data.clone();
    for &(ix, kind, direction) in ixes.iter().rev() {
        let mut new_data = Vec::with_capacity(data.len());
        match kind {
            Kind::Id => {
                let mut buffer = vec![];
                for row in data.chunks(chunk.row_width) {
                    buffer.push((row[ix], row));
                }
                match direction {
                    Direction::Ascending => buffer.sort_by(|&(key_a, _), &(key_b, _)| key_a.cmp(&key_b)),
                    Direction::Descending => buffer.sort_by(|&(key_a, _), &(key_b, _)| key_b.cmp(&key_a)),
                }
                for (_, row) in buffer.into_iter() {
                    new_data.extend(row);
                }
            }
            Kind::Number => {
                let mut buffer = vec![];
                for row in data.chunks(chunk.row_width) {
                    buffer.push((to_number(row[ix]), row));
                }
                // TODO NaN can cause panic here
                match direction {
                    Direction::Ascending => buffer.sort_by(|&(key_a, _), &(key_b, _)| key_a.partial_cmp(&key_b).unwrap()),
                    Direction::Descending => buffer.sort_by(|&(key_a, _), &(key_b, _)| key_b.partial_cmp(&key_a).unwrap()),
                }
                for (_, row) in buffer.into_iter() {
                    new_data.extend(row);
                }
            }
            Kind::Text => {
                let mut buffer = vec![];
                for row in data.chunks(chunk.row_width) {
                    buffer.push((&strings[row[ix+1] as usize], row));
                }
                match direction {
                    Direction::Ascending => buffer.sort_by(|&(ref key_a, _), &(ref key_b, _)| key_a.cmp(key_b)),
                    Direction::Descending => buffer.sort_by(|&(ref key_a, _), &(ref key_b, _)| key_b.cmp(key_a)),
                }
                for (_, row) in buffer.into_iter() {
                    new_data.extend(row);
                }
            }
        }
        data = new_data;
    }
    Chunk{data: data, row_width: chunk.row_width}
}
```

The query language is basically feature-complete at this point. I'm missing state and stratification but I don't *need* either of those to bootstrap. The compiler has become a bit of a mess and is probably full of bugs though. I think the next thing I want to do is to get some basic editor integration working to make debugging easier and then write some actual programs. If everything works well enough, it may be worth trying to bootstrap right away instead of cleaning up the existing compiler.

## Negation

I totally forgot I needed negation. I added it only at the level of individual clauses, like standard datalog. This means I can write:

```
?p:text works at alice crop but not evil eve studios
+
?p works at "alice corp" for _ USD
! ?p works at "evil eve studios" for _ USD
```

But I can't directly write:

```
?p:text only works at alice crop
+
?p works at "alice corp" for _ USD
! {
  ?p works at ?c for _ USD
  c != "alice corp"
}
```

Most query languages don't support that anyway, so maybe it's ok?

I piggybacked negation onto primitives too. The compiler is getting gnarly. Might be time for a cleanup soon.

## Parsing

I tried to work on editor integration and ended up just [procrastinating](https://github.com/jamii/imp/blob/master/data/csa.imp) a lot. This week I changed tack and started bootstrapping instead.

The current parser is a [nasty mess](https://github.com/jamii/imp/blob/2e344bcdb4fd288c37052b8340cfad3b0dfc6878/src/bootstrap.rs#L626-L763) of regular expressions. This is partly because I care more at this stage about getting something working than making it pretty, but it's also because Imp doesn't really need or benefit from the traditional parsing formalisms.

One reason for that is that I want parsing errors to be locally contained. In most languages, deleting a single parenthesis can make the whole program unparseable. This is a disaster for live programming. In Imp, the high-level structure of the program is a [regular language](https://en.wikipedia.org/wiki/Regular_language). There are only three levels of nesting (program -> view -> member -> clause) so each one gets to use a unique delimiter. The first half of the parser just splits up the program at these delimiters, not caring about the text between them. This means that syntax errors can only break things locally eg missing a view delimiter mashes two views together but leaves all the other views intact.

The second reason is that the clauses themselves don't have a very well rigid grammar. Given the clause `most "cats" prefer ?x` the parser picks out the bindings `"cats"` and `?x` and then converts the remainder into the view name `most _ prefer _`. Handling that in a traditional grammar is mildly unpleasant.

So instead the Imp grammar is given by tree of regular expressions:

```
?parent:text contains ?child:text found by capture ?n:number of ?regex:text
=
"program" "view" 0 "(.+\n?)+"
"view" "head" 0 "^.*"
"view" "insert" 1 "\n\+((\n[^\+-=].*)+)"
"view" "remove" 1 "\n-((\n[^\+-=].*)+)"
"view" "input" 1 "\n=(.*(\n[^\+-=].*)+)"
"head" "variable with kind" 2 "(^|\s)(\?[:alnum:]*:[:alnum:]*)"
"variable with kind" "kind" 1 ":([:alnum:]*)"
"variable with kind" "variable" 1 "\?([:alnum:]*)"
"insert" "clause" 1 "\n(.*)"
"remove" "clause" 1 "\n(.*)"
"clause" "negation" 0 "^! "
"clause" "clause body" 2 "^(! )?(.*)"
"clause body" "binding" 2 "(^|\s)(_\S*|\?[:alnum:]*|#[:digit:]+|-?[:digit:]+(\.[:digit]+)?|\x22(\\\x22|[^\x22])*\x22)"
"binding" "unbound" 0 "^_\S*$"
"binding" "variable" 1 "^\?([:alnum:]*)$"
"binding" "id" 0 "^#[:digit:]+$"
"binding" "number" 0 "^-?[:digit:]+(\.[:digit]+)?$"
"binding" "text" 0 "^\x22(\\\x22|[^\x22])*\x22$"
"input" "import" 0 "^.*"
"import" "filename" 1 "^\s*(\S*)"
"import" "cols" 1 "^\s*\S*(.*)"
"cols" "col" 1 "\s*(\S*)"
"input" "row" 1 "\n(.*)"
"row" "value" 2 "(^|\s)(#[:digit:]+|-?[:digit:]+(\.[:digit]+)?|\x22(\\\x22|[^\x22])*\x22)"
"value" "id" 0 "#[:digit:]+"
"value" "number" 0 "-?[:digit:]+(\.[:digit]+)?"
"value" "text" 0 "\x22(\\\x22|[^\x22])*\x22"
```

The result of parsing is a similar tree, where each node is identified by rule creating it and by the byte indices at which it starts and ends in the program text.

```
child ?ck:text ?ca:number ?cz:number of ?pk:text ?pa:number ?pz:number has text ?c:text
+
outside says ?pk ?pa ?pz has child ?ck ?ca ?cz with text ?c
+
?pk contains ?ck found by capture ?n of ?re
child ?pk ?pa ?pz of _ _ _ has text ?p
capture ?n of result _ of ?p searched by ?re is at ?ra to ?rz
?ca = ?pa + ?ra
?cz = ?pa + ?rz
the text at ?ra to ?rz in ?p is ?c
```

I also added some basic debugging support which watches the file and prints results like:

```
View 4: "child _ _ _ of _ _ _ has text _"
[Text, Number, Number, Text, Number, Number, Text]
"value" 184 193 "row"   184 213 "\"program\""
"value" 194 200 "row"   184 213 "\"view\""
"value" 201 202 "row"   184 213 "0"
"value" 203 213 "row"   184 213 "\"(.+\\n?)+\""
"value" 214 220 "row"   214 235 "\"view\""
"value" 221 227 "row"   214 235 "\"head\""
...
```

That's the whole parser. It isn't pretty and there is some unpleasant repetition in the grammar, but every attempt I've made to reduce that repetition has resulted in something that is more complicated overall. When the whole parser consists of 28 rules and 6 lines of logic it's hard to gain anything from adding further abstraction.

The [nasty mess](https://github.com/jamii/imp/blob/2e344bcdb4fd288c37052b8340cfad3b0dfc6878/src/bootstrap.rs#L626-L763) in the Rust version expresses more or less the same logic but is much more verbose. The reason for that is that I started by writing down the types I wanted to output:

``` rust
#[derive(Clone, Debug)]
pub struct Program {
    pub ids: Vec<ViewId>,
    pub schedule: Vec<usize>,
    pub schemas: Vec<Vec<Kind>>,
    pub views: Vec<View>,
}

#[derive(Clone, Debug)]
pub enum View {
    Input(Input),
    Query(Query),
    Union(Union),
}

#[derive(Clone, Debug)]
pub struct Input {
    pub tsv: Option<(String, Vec<usize>)>,
    pub rows: Vec<Vec<Value>>,
}

#[derive(Clone, Debug)]
pub struct Query {
    pub clauses: Vec<Clause>,
    pub select: Vec<VariableId>,
}

#[derive(Clone, Debug)]
pub struct Union {
    pub members: Vec<Member>,
}

#[derive(Clone, Debug)]
pub enum Member {
    Insert(ViewId),
    Remove(ViewId),
}

#[derive(Clone, Debug)]
pub struct Clause {
    pub view: ViewId,
    pub bindings: Vec<Binding>,
    pub over_bindings: Vec<(Binding, runtime::Direction)>,
}
```

By starting with a heterogenous tree of custom types I had *already missed* the opportunity to build a simple, data-driven parser like the Imp version. What's more, I can easily add information to the Imp version in a way that would require modifying types in the Rust version:

```
head ?va:number ?vz:number is named ?n:text
+
child "head" ?va ?vz of _ _ _ has text ?v
"head" contains "variable with kind" found by capture _ of ?re
?v with ?re replaced by "$1_" is ?n

clause ?va:number ?vz:number is named ?n:text
+
child "clause body" _ _ of "clause" ?va ?vz has text ?v
"clause" contains "binding" found by capture _ of ?re
?v with ?re replaced by "$1_" is ?n

view ?n:text is primitive
=
"_ = _ + _"
"_ = sum(_)"
"row _"
"_ < _"
"_ <- _"
"_ <<- _"
"min"
"result _ of _ split by _ is at _ to _ breaking at _"
"result _ of _ searched by _ is at _ to _"
"capture _ of result _ of _ searched by _ is at _ to _"
"_ with _ replaced by _ is _"
"the text at _ to _ in _ is _"
"_ has length _"

clause ?va:number ?vz:number is primitive
+
clause ?va ?vz is named ?n
view ?n is primitive

clause ?va:number ?vz:number is negated
+
child "negation" _ _ of "clause" ?va ?vz has text _

clause ?va:number ?vz:number is finite
+
clause ?va ?vz is named _
! clause ?va ?vz is primitive
! clause ?va ?vz is negated
```

In a pointerful language like Rust or Clojure or Javascript I would have to spend time deciding where this data lives and how to access it. Any change to the organization of the pointer graph would require rewriting all the code that traverses it. In Imp I just define the data and refer to it directly. I strongly suspect that this is going to be a major improvement.

# Mid-mortem

At about 400 lines of Imp code, the bulk of the parser/compiler is finished and working. Progress has been halting - in part because I've been distracted by other work and by having to leave the country to get a new visa (going from two 30" monitors on a standing desk to a 12" laptop on a wooden chair has not been kind to my body) - but I think I'm at the point now where I've learned all I'm going to and the rest of the work is going to be just more of the same.

So far, [imp.imp](https://github.com/jamii/imp/blob/master/data/imp.imp) can correctly parse programs, compile input and union views and generate join trees for query views. The primitive scheduling for query views exists but is incomplete and the action list is missing entirely. I'm also missing the Rust code that will take the plans generated by imp.imp and assemble them into the corresponding runtime data-structures. If I plowed ahead without changing anything, I'm guessing the finished version would be ~600 lines of Imp and ~100 lines of Rust (the native compiler is ~1000 lines of Rust and ~200 lines of PEG grammar).

## Performance

The Rust version takes ~2ms to parse imp.imp and ~2ms to compile imp.imp. The Imp version takes ~60ms to parse itself and ~60ms to (incompletely) compile itself. (The current parser in the Rust version is a [rust-peg](https://github.com/kevinmehall/rust-peg) parser contributed by [wtaysom](https://github.com/wtaysom). For a fairer comparison, the old regex mess takes ~50ms.)

Oprofile and valgrind agree that a large chunk of the runtime is spent in applying primitives and sorting. Looking at the individual view timings, most of the time is spent in just a few views and those all use either recursion or regex primtives or both, which matches up with the profiling results.

Recursion is a problem on two fronts. Firstly, I haven't implemented [semi-naive evaluation](http://infolab.stanford.edu/~vassalos/cs345_98/datalog3.ps) so every iteration of a recursive view has to recalculate the entire view so far. Secondly, I don't maintain indexes between executions so every one of those iterations requires re-sorting everything. This means lots of extra sorting and lots of redundant calls to primitives.

Valgrind puts 15% of the total runtime in the primitive `capture _ of result _ of _ searched by _ is at _ to _` but this is likely a result of the ~15000 calls (!) to this primitive, rather than a sign that regexes themselves are slow to run. Compiling regexes *is* slow but I already [added a regex cache](https://github.com/jamii/imp/commit/982afa85fb75f9bcf822008f422a582ea95b7a0d), a strategy [used by many high-level languages](https://msdn.microsoft.com/en-us/library/8zbs0h2f%28v=vs.110%29.aspx). Just hitting the regex cache is responsible for 5% of the total runtime but, again, 15000 calls is the root problem here.

Valgrind puts another 8% at the feet of `_ with _ replaced by _ is _` with only ~750 calls. I'm don't know why it is so much more expensive than the regex search alone.

I expected string allocation to be an issue but it barely even registers. The way I handle strings still needs more thought, but it doesn't look to be at all urgent.

Overall, I'm pleasantly surprised by the performance. I expected it to be much slower than it is and I expected the culprits to be spread out all over the place. As it is, it looks like I can get huge improvements just from maintaining indexes intelligently and implementing semi-naive evaluation, both of which I was planning to do anyway.

## Expressiveness

Most of the code has been pretty straightfoward to write.

Being able to aggregate over arbitary subqueries has been really useful eg:

```
union node ?n:number has key ?f:number = ?v:number
+
field ?f of node ?n has variable _ and kind ?k
field ?f2 of node ?n has variable _ and kind ?k2
?f2 < ?f
values of kind ?k2 have width ?w
?v = sum(?w) over ?f2 ?k2
```

This calculates, for each field, the sum of the widths of all the earlier fields. In all versions of Eve so far, doing that would require creating another intermediate view just to track which fields are earlier than others, because aggregates can only be applied to entire views:

```
union node ?n:number field ?f:number is ahead of field ?f2:number with width ?w:number
+
field ?f of node ?n has variable _ and kind _
field ?f2 of node ?n has variable _ and kind ?k
values of kind ?k have width ?w
?f2 < ?f

union node ?n:number has key ?f:number = ?v:number
+
union node ?n field ?f is ahead of field ?f2 with width ?w
?v = sum(?w) over ?f2
```

I have a similar situation with negation. Negation can currently only be applied to a single clause so I often end up creating intermediate views instead eg:

```
clause ?c:number of node ?n:number is unjoined in step ?s:number
+
clause ?c of node ?n is finite
?s <- 0
+
node ?n reaches step ?s
clause ?c of node ?n is unjoined in step ?s2
?s = ?s2 + 1
! clause ?c of node ?n is joined to clause _ in step ?s2

# in use later
clause ?c of node ?n is unjoined in step ?s
```

Which would be more directly expressed with negation over a subquery:

```
# direct use, no intermediate view
! ?s2 [
  clause ?c of node ?n is joined to clause _ in step ?s2
  ?s2 < ?s
]
```

This is only a small improvement in this case, but elsewhere in imp.imp it would save a lot of boilerplate because the intermediate views often copy lots of context from the main view in order to ensure that they have a finite number of results. It also allows for a more efficient interpretation since an intermediate view has to be generated in full but a negation can shortcircuit as soon as it finds one result.

Another approach to this general problem would be to allow views with infinite results so long as they are only used in contexts that guarantee the end result if finite. This is leaning away from the restrictions that make datalog so much easier to evaluate than prolog and kin, so it would require very careful though. The upside would be that it would provide a mechanism for user-defined functions as well as an implementation of aggregates and negations over sub-queries.

There are also a few cases where disjunction would be useful, but it's not nearly as common as I would have expected.

Finally, I made a big fuss about the syntax I added for non-monotonic reasoning and so far I have used it exactly 0 times. It's [roughly equivalent](https://twitter.com/arntzenius/status/658735928803008512) in power to negation and in every place I could have used it so far I've found negation to be the cleaner option.

## Context

I don't really have a good name for this category yet, but it is a pattern that seems to be important.

In the Rust version, the compiler operates on one query at a time. I think of the function stack as defining a context in which the code is currently operating. Imp doesn't have a function stack, or any kind of nesting, so every view has to track it's whole context. Notice how the node ?n is not really being used in the following view, it's just there for context:

```
in node ?n:number wave ?w:number primitive ?p:number is scheduled on subtree ?t:id
+
in node ?n wave ?w primitive ?p is unscheduled
node ?n has subtree ?t with root _
! node ?n clause ?p cannot be run on subtree ?t
in node ?n wave ?w subtree ?t has ?numc clauses
min over ?numc ?t ?p
```

This is kind of a refactoring problem. In a normal language I can write code as if there is only one node in the world and then reuse that function across multiple nodes. It would be useful to be able to do the same in Imp - write a set of views as if there was only one node and then select them all and declare a node context to be threaded throughout.

I think there are some problems with the way that context works in most languages that I would like to avoid. In particular, the tools given to you are primarily hierachical data-structures (pointers, structs, hashtables etc are all one-way mappings) and the hierachichal function call-graph. But context doesn't necessarily decompose nicely into a tree-like structure. It's common that different contexts overlap but don't contain each other, and one symptom of this is when you find that state sharing doesn't respect your tree-like breakdown eg functions that act on a single node have to access type information that comes from multiple nodes.

In my mind, context looks like [plate notation](https://www.google.co.uk/search?q=plate+notation+graphical+model&client=ubuntu&hs=HX5&channel=fs&biw=1118&bih=561&source=lnms&tbm=isch&sa=X&ved=0ahUKEwirl6uBorHJAhVBAxoKHYLdCmk4FBD8BQgGKAE), where each plate surrounds a set of views that are paremeterised on some shared field. For each plate, you could choose to see all the data or instead fix the shared field to a single value. For example, in the Imp compiler I might want to see the whole dataflow or instead narrow it to a single query node or a single view step in the plan for a node. This narrowing is similar to the views you can get by stepping in and out of functions in a normal language, but following the shape of the data flow rather than the control flow.

Navigating context will become more important in larger programs, but there is a related problem that is already unpleasant in the compiler. I often want to describe one instance of a context as being based on another, with some small changes. For example, for each step in the query plan the state is mostly the same as the previous step. At the moment, maybe 30% of the code in imp.imp is just boilerplate copying of state and it's going to be worse in the parts that are yet to be written.

```
in node ?n:number wave ?w:number primitive ?p:number is unscheduled
+
?w <- 0
clause ?p of node ?n is primitive
+
in node ?n wave ?w2 primitive ?p is unscheduled
in node ?n wave ?w2 primitive _ is scheduled on subtree _
! in node ?n wave ?w2 primitive ?p is scheduled on subtree _
?w = ?w2 + 1

in node ?n:number wave ?w:number clause ?c:number is unjoined
+
?w <- 0
clause ?c of node ?n is finite
+
in node ?n wave ?w2 clause ?c is unjoined
in node ?n wave ?w2 primitive _ is scheduled on subtree _
! in node ?n wave ?w2 clause ?c is joined
?w = ?w2 + 1
```

[Bloom](http://bloom-lang.net/) provides one way of dealing with this by providing syntax sugar for views which change over time, but this is a very limited subset of the problem. I already run into it in calculating the join tree, in scheduling primitives within a query, in scheduling joins for each primitive application and in scheduling actions within each join. Lots of nested, overlapping contexts that don't fit well into a single timeline with a forgotten past.

I strongly suspect that this context problem is the most important on this list, and is vital for making Imp a practical language.

## Structure

I notice that I get often get lost while working on the compiler. I think there are some accidents of syntax that have conspired to force me to context switch a lot more than I really need to.

At the moment, each view is defined in a single place together with all the logic that contributes to it:

```
child ?ck:text ?ca:number ?cz:number of ?pk:text ?pa:number ?pz:number has text ?c:text
+
outside says ?pk ?pa ?pz has child ?ck ?ca ?cz with text ?c
+
?pk contains ?ck found by capture ?n of ?re
child ?pk ?pa ?pz of _ _ _ has text ?p
capture ?n of result _ of ?p searched by ?re is at ?ra to ?rz
?ca = ?pa + ?ra
?cz = ?pa + ?rz
the text at ?ra to ?rz in ?p is ?c
```

This has some unfortunate side effects.

First is that the schema definition is spread out all over the program. In Rust I tend to push all the important type definitions to the top of the file so that the reader can see the overall structure at a glance. Even in Clojure, I tend to start each namespace with a long comment describing the data. In Imp I can't do that, and not only does that make it harder to refer to but I also end up with a messier schema.

Second, each query can only feed into one view, so I tend to push lots of data into one view rather than repeat the body of the query multiple times. This results in a somewhat denormalised schema which is harder for me to remember later on and harder to refactor when I want to change something.

I would be better off with the structure that we used back in older versions of Eve, where each query body can push data into multiple views:

```
?ck:text ?ca:number ?cz:number has parent ?pk:text ?pa:number ?pz:number
?ck:text ?ca:number ?cz:number has text ?c:text

outside says ?pk ?pa ?pz has child ?ck ?ca ?cz with text ?c
=>
?ck ?ca ?cz has parent ?pk ?pa ?z
?ck ?ca ?cz has text ?c

?pk contains ?ck found by capture ?n of ?re
?pk ?pa ?pz has text ?p
capture ?n of result _ of ?p searched by ?re is at ?ra to ?rz
?ca = ?pa + ?ra
?cz = ?pa + ?rz
the text at ?ra to ?rz in ?p is ?c
=>
?ck ?ca ?cz has parent ?pk ?pa ?z
?ck ?ca ?cz has text ?c
```

This also removes the need for the ugly assignment primitive `_ <- _`, since I can use constants and repeated variables in the output.

I'm also less keen on the sentence-like syntax experiment. While it makes the meaning of individual views easier to express, it hinders pattern matching when reading large amounts of code. I may adopt [LogicBlox' parametric relations](http://2015.splashcon.org/event/splash2015-splash-i-shan-shan-huang-talk) instead which enable some nice syntactic sugar. Not sure whether their approach to aggregation is general enough to handle the compiler though, so I'll still have to figure that out separately.

```
parent(kind, number, number) = (kind, number, number)
text(kind, number, number) = text

contains-child(pk, ck) = (re, n)
text(pk, pa, pz) = p
search(p, re, n) = (ra, rz)
pa + ra = ca
pa + rz = cz
text-at(p, ra, rz) = c
=>
parent(ck, ca, cz) = (pk, pa, pz)
text(ck, ca, cz) = c
```

With all the of the surrounding text gone those variable names will need to be longer, but otherwise this looks pretty reasonable. That may have something to do with the decade I've spent looking at such patterns though.

## Correctness

Annoyingly, I didn't think to keep a record of the bugs I found while writing the compiler. My rough recollection is that most code worked on the first run. Most of the bugs I ran into were in the implementation itself and not the Imp code that I wrote. All but one of the Imp bugs that I can remember were caused either by typing errors or by incorrectly terminated loops. The former could all be caught by checking for unused variables and basic type-checking. The latter happened mostly in the boilerplate state changes for the primitive scheduling and might be avoided by removing the boilerplate, as discussed earlier in *context*. Finally, the only logic bug I can recall is still in the code at the time of writing - I forgot to track when the outputs of primitives may be used so queries with multiple chained primitives don't compile.

On the other hand, checking that the code is correct tends to be pretty hard. Data is scattered across many views and I don't have a good way of viewing yet it other than printing out the entire database. So while I don't spend much time debugging I do spend a lot of time scrolling back and forth through the output trying to figure out if I got the correct results.

There isn't much in the way of error handling yet but I'm quite happy with the way it has worked out so far. For the most part, I've been able to write the happy path first and then add error handling without having to modify the happy path. For example, if the compiler cannot find a join tree for a given query, it will simply stop short. A later view checks to see which views have not been finished:

```
clause ?c:number is not joined in node ?n:number
+
clause ?c of node ?n is unjoined in step 0
! clause ?c of node ?n is joined to clause _ in step _
! node ?n has root clause ?c
```

By contrast, the Rust version immediately throws an exception inline if it can't find a join tree. This makes it much harder to build a resilient compiler (I don't seem to have written about this anywhere, but the current Eve compiler can partially compile partially correct code and continue running without losing state).

## Tooling

[wtaysom](https://github.com/wtaysom)'s new parser at least gives me line numbers and readable errors, which is already a huge improvement on my regex mess. Adding syntax highlighting, checking for unused variables and type-checking would catch most of my remaining mistakes.

Showing the data for whatever view the cursor is currently inside would provide much faster feedback, especially since I could then use throwaway queries to ask questions like `clause ?c of node 3 is joined to clause _ in step ?s`. I would also like an overview of the whole dataset, since a common symptom of mistakes is that some views will suddenly have 0 rows.

When a view is empty, I often resort to tracing the data back by hand. [Why-not provenance](https://www.lri.fr/~herschel/resources/bidoit_BDA2013.pdf) would automate this process. Similarly, when trying to figure out why a view has unexpected results in it, [why provenance](https://users.soe.ucsc.edu/~tan/papers/2001/whywhere.pdf) would be much better than simulating it by hand.

## Types

For the most part, I've been perfectly happy with only strings and numbers. The sole exception is when calculating subtrees of the join tree. Since there are many paths by which I can generate the same subtree I want to ensure that I remove duplicates. The natural way to do this would be to use the set of nodes in the subtree as the key but I don't have a set type. Instead I'm currently (ab)using the id type as a bitset:

```
node ?n:number has subtree ?t:id with root ?r:number
+
clause ?r of node ?n is unjoined in step 0
?t = set bit ?r of #0
+
clause ?c of node ?n is joined to clause ?p in step _
node ?n has subtree ?t2 with root ?r
1 = get bit ?p of ?t2
?t = set bit ?c of ?t2
```

This is a hacky solution but so far it's the only time I've felt the need for a complex type. I'll wait untill it happens at least a few more times before I start thinking about how to solve it.

## Implementation

At the start of this diary I explained that I was making the assumption that I could get away without maintaining indexes because I would mostly be processing data in bulk. This has gotten me pretty far, but it won't let me take advantage of semi-naive or incremental evaluation - both of which rely on the assumption that making one of the inputs much smaller greatly reduces the work needed to answer the query, an assumption which does not hold if all the inputs have to be re-indexed anyway.

Eve is also moving more towards a database-like system where the focus is on external user-driven mutation rather than internal, programmatic state changes. I would like to share the runtime if practical, so that also violates some of my early assumptions.

My handling of primitives, especially aggregates, is still somewhat dubious and I would like to have a proper theory around that rather than just a crude heuristic. I also have some new ideas around the relationship between [Tetris](http://arxiv.org/pdf/1404.0703.pdf), [Triejoin](http://arxiv.org/abs/1210.0481) and Yannakakis algorithm that may bear fruit.

All of that points towards revisiting the query planner with an eye towards incremental evaluation and a better theoretical basis for planning aggregates.

## Summary

Things that worked out:

* Compact data layout, with strings stored elsewhere but hashes stored inline
* Hypertrees as a basis for planning queries
* Yannakikis algorithm
* Primitives as infinite relations
* Bootstrapping
* Error handling out-of-band

Things that need work:

* Syntax
* Error checking / static analysis
* Viewing data
* Negation

Things that need thought:

* Aggregates
* Incremental evaluation
* Index data-structures
* Context / time / mutation
* Infinite views?
* Provenance

I think I'm going to look at the aggregates first, because I have a lot of half-formed ideas around query planning that may make incremental evaluation and provenance easier too.

## Theory

I spent two weeks carefully reading all the recent work on join algorithms and eventually reached a tipping point where suddenly it all made sense. I've written most of an article explaining the rough ideas in simpler terms, but before publishing it I want to spend some time trying to simplify the implementation and proof too.

## Unsafe

I also spent a week or two exploring data-structures for the indexes. I tried building a [HAMT](https://en.wikipedia.org/wiki/Hash_array_mapped_trie)-like structure in unsafe Rust. I learned a lot about how unsafe Rust works and how to use valgrind and gdb, but eventually concluded that it just isn't worth the time it would take to finish it.

Using the same layout as [Champ](http://michael.steindorfer.name/publications/oopsla15.pdf) would be far easier and produce far less segfaults. I haven't seen a comparison between the original C++ HAMT and the various descendants in managed languages so it's hard to say how much the extra pointer indirections cost. I wonder if there is some way to estimate the difference without actually having to implement both...

# Compiling

Imp is currently an interpreter. The overhead of interpreting query plans is hard to determine exactly, but the execution time is dominated by sorting and the sort function is ~35% faster if I hardcode the data layout for a specific table, so it's certainly non-trivial.

The current runtime works table-at-a-time to amortise the overhead of interpretation. For example, when applying functions like `+` there is a single dispatch to find the matching code and then a loop over the whole table:

``` rust
match (*self, input_ixes) {
    (Primitive::Add, [a, b]) => {
        for row in chunk.data.chunks(chunk.row_width) {
            data.extend(row);
            data.push(from_number(to_number(row[a]) + to_number(row[b])));
        }
    }
    ...
```

All the new join algorithms I have been researching work tuple-at-a-time so it's not possible to amortise the overhead in the same way. The algorithms are generally simple to write for a specific case, but building an interpreter that efficiently executes any case is difficult. It would be far easier to just emit code for each query, but Rust doesn't make that easy.

In fact, there would be a lot of things that would get easier if was just emitting code in the same language. I could let the existing language handle data layout and type checking. I would be able to use the existing libraries directly instead of [arduously wrapping them](https://github.com/jamii/imp/blob/1c41bdd4f0d5372be307d9d483caf8e8e6e9a1e8/src/primitive.rs) and I could use the repl and other tools with Imp.

This is what I did for most of the early versions of Eve. The problem is that the languages that make this kind of meta-programming practical tend to also have poor control over data layout and very opaque performance models. It's possible to [hack around](http://objectlayout.org/) the limitations but you end up in much the same boat as before - implementing your own data layout and type system that can't play with the existing standard library.

<blockquote class="twitter-tweet" lang="en"><p lang="en" dir="ltr">Heartening to see the focus on multi-stage programming in <a href="https://t.co/iSqkk9fmtW">https://t.co/iSqkk9fmtW</a>. There is a distinct lack of good staging languages.</p>&mdash; Jamie Brandon (@jamiiecb) <a href="https://twitter.com/jamiiecb/status/676921026601725953">December 16, 2015</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

I ended up using Rust after one too many evenings of wanting to stab Hotspot in the face. Back when I made the decision Mike Innes [argued](https://groups.google.com/forum/#!searchin/eve-talk/julia/eve-talk/5EifQQUHQUw/u3U_ERkbKFcJ) for using Julia instead. Of the objections that I brought up, some have since been fixed and some look like they are going to be fixed in the near future. The remainder (no interior pointers, layout restricted by the gc) seem like a fair trade for potentially removing the interpreter overhead. So I played around with Julia over the holidays.

## Return of the Yannakakis

The first thing I tried in Julia is porting part of the current Imp runtime - enough to hand-compile one of the Chinook queries.

Tables in the Rust version are a `Vec<u64>` with all the data layout being handled by the Imp compiler a layer above. Julia is dynamically typed so I can just use a vector of tuples and let the Julia compiler figure out the layout.

``` julia
ids() = ids(1000000)
ids(n) = rand(1:n, n)

eg_table = [(a,b) for (a,b) in zip(ids(), ids())]

typeof(eg_table
# Array{Tuple{Int64,Int64},1}
```

In Julia, immutable types like tuples are treated as value types. That means that this `Array{Tuple{Int64,Int64},1}` is a single contiguous allocation, not an array of pointers to tuple objects. To get this in Clojure or Javascript I would have to use a flat array and then write all my own sort functions from scratch to account for the rows. In Julia I can rely on the compiler to handle this for me.

``` julia
f() = begin
  xs = [(a,b) for (a,b) in zip(ids(), ids())]
  @time sort(xs, alg=QuickSort)
end

# 0.193217 seconds (10 allocations: 15.259 MB, 0.39% gc time)
```

This is on par with the stdlib sort in Rust and ~2x slower than the radix sort used in Imp. Also note that only 10 allocations were reported. The compiler is smart enough to reuse allocations for the boxed tuples rather than creating 1M temporary tuples on the heap.

We need to build up some basic relational functions.

``` julia
project(xs, ykey) = begin
  ys = Vector(0)
  for x in xs
    push!(ys, x[ykey])
  end
  sort!(ys, alg=QuickSort)
  dedup_sorted(ys)
end

f() = begin
  xs = [(a,b) for (a,b) in zip(ids(), ids())]
  @time project(xs, [2])
end

# 1.418393 seconds (4.00 M allocations: 220.172 MB, 8.61% gc time)
```

That's not good. Far too slow, far too many allocations and, worst of all, the returned value is a `Vector{Any}` ie an array of pointers to tuples.

First, let's fix the return type. By default `Vector` returns a `Vector{Any}`, but we can specify the type if we want something else. Since types are first-class values in Julia we can just pass the return type as an argument.

``` julia
project(xs, ykey, ytype) = begin
  ys = Vector{ytype}(0)
  for x in xs
    push!(ys, x[ykey])
  end
  sort!(ys, alg=QuickSort)
  dedup_sorted(ys)
end

f() = begin
  xs = [(a,b,c) for (a,b,c) in zip(ids(), ids(), ids())]
  project(xs, [1,2], Tuple{Int64, Int64})
end

# 0.645461 seconds (5.00 M allocations: 254.867 MB, 10.12% gc time)
```

Next, we need to give the compiler enough information that it can reuse the allocation for `x[ykey]`. Suppose we made it's job easy by pulling out the critical function and by hardcoding the key:

``` julia
reshape(xs, ys, ykey) = begin
  for x in xs
    push!(ys, (x[1], x[2]))
  end
end

project(xs, ykey, ytype) = begin
  ys = Vector{ytype}(0)
  reshape(xs, ys, ykey)
  sort!(ys, alg=QuickSort)
  dedup_sorted(ys)
end

f() = begin
  xs = [(a,b,c) for (a,b,c) in zip(ids(), ids(), ids())]
  @time project(xs, [1,2], Tuple{Int64, Int64})
end

#   0.216851 seconds (42 allocations: 34.001 MB, 0.61% gc time)
```

That's much better. Now I just have to figure out how to make a hardcoded version of reshape for each key. I could (and did) do it with macros, but macros have a tendency to spread and turn everything else into macros. It would be nice if we could just piggyback on the existing specialisation machinery, and Julia recently gained the ability to do just that through the combination of two new features.

The first new feature is [value types](http://docs.julialang.org/en/release-0.4/manual/types/#value-types). `Val{x}` takes an immutable value `x` and turns it into a type, which allows us to specialise on values as well as types.

``` julia
reshape{T}(xs, ys, ykey::Type{Val{T}}) = begin
  for x in xs
    push!(ys, construct(ykey, x))
  end
end

project(xs, ykey, ytype) = begin
  ys = Vector{ytype}(0)
  reshape(xs, ys, Val{ykey})
  sort!(ys, alg=QuickSort)
  dedup_sorted(ys)
end
```

The second feature is [generated functions](http://docs.julialang.org/en/release-0.4/manual/metaprogramming/#generated-functions) which allow the programmer to emit custom code for each specalisation of the function.

``` julia
@generated construct{T}(key::Type{Val{T}}, value) = begin
  ixes = key.parameters[1].parameters[1]
  :(begin
      tuple($([:(value[$ix]) for ix in ixes]...))
    end)
end
```

This is true [multi-stage programming](http://www.cs.rice.edu/~taha/MSP/), something which is painful to achieve with macros and eval alone.

``` julia
f() = begin
  xs = [(a,b,c) for (a,b,c) in zip(ids(), ids(), ids())]
  @time project(xs, (1,2), Tuple{Int64, Int64})
end
# 0.208217 seconds (41 allocations: 34.001 MB, 1.06% gc time)
```

Just as fast as the hardcoded version.

Not so happy if we try some other types though.

``` julia
f() = begin
  xs = [(a,Float64(b),c) for (a,b,c) in zip(ids(), ids(), ids())]
  @time project(xs, (1,2), Tuple{Int64, Float64})
end
# 2.746427 seconds (98.79 M allocations: 2.234 GB, 14.23% gc time)
```

I'm not sure what's going on here yet. I [started a thread](https://groups.google.com/forum/#!topic/julia-users/4L693Z8qePw) on the mailing list. I suspect either a bug in the boxing analysis or some heuristics around when to specialise `push!`.

So tuples don't always work nicely, but fortunately Julia is the kind of language where we can make our own tuples.

``` julia
abstract Row

macro row(name, types)
  types = types.args
  :(immutable $(esc(name)) <: Row
      $([:($(symbol("f", i))::$(types[i])) for i in 1:length(types)]...)
    end)
end

@generated cmp_by_key{R1 <: Row, R2 <: Row}(x::R1, y::R2, xkey, ykey) = begin
  xkey = xkey.parameters[1].parameters[1]
  ykey = ykey.parameters[1].parameters[1]
  @assert(length(xkey) == length(ykey))
  :(begin
      $([:(if !isequal(x.$(xkey[i]), y.$(ykey[i])); return isless(x.$(xkey[i]), y.$(ykey[i])) ? -1 : 1; end) for i in 1:length(xkey)]...)
      return 0
    end)
end

@generated Base.isless{R <: Row}(x::R, y::R) = begin
  key = [symbol("f", i) for i in 1:length(fieldnames(R))]
  last = pop!(key)
  :(begin
      $([:(if !isequal(x.$k, y.$k); return isless(x.$k, y.$k); end) for k in key]...)
      return isless(x.$last, y.$last)
    end)
end

@generated construct{C,K}(constructor::Type{C}, key::Type{Val{K}}, value) = begin
  constructor = constructor.parameters[1]
  fields = key.parameters[1].parameters[1]
  :(begin
      $constructor($([:(value.$field) for field in fields]...))
    end)
end

...

f() = begin
  xs = [A(a,b,c) for (a,b,c) in zip(ids(), ids(), ids())]
  @time project(xs, (:f1,:f2), B)
end

# 0.211331 seconds (41 allocations: 34.001 MB, 1.09% gc time)
```

So it does the job. Hopefully this is a temporary fix, because tuples are nicer ergonomically, but it's reassuring that we have this level of access to low-level primitives.

Some merge-joins:

``` julia
join_sorted_inner{X,Y,Z,XK,YK,ZK1,ZK2}(
  xs::Vector{X}, ys::Vector{Y}, ztype::Type{Z},
  xkey::Type{Val{XK}}, ykey::Type{Val{YK}}, zkey1::Type{Val{ZK1}}, zkey2::Type{Val{ZK2}}
  ) = begin
  zs = Vector{Z}(0)
  xi = 1
  yi = 1
  while (xi <= length(xs)) && (yi <= length(ys))
    x = xs[xi]
    y = ys[yi]
    c = cmp_by_key(x, y, xkey, ykey)
    if c == -1
      xi += 1
    elseif c == 1
      yi += 1
    else
      xj = xi
      yj = yi
      while (xj <= length(xs)) && (cmp_by_key(x, xs[xj], xkey, xkey) == 0)
        xj += 1
      end
      while (yj <= length(ys)) && (cmp_by_key(y, ys[yj], ykey, ykey) == 0)
        yj += 1
      end
      for xk in xi:(xj-1)
        for yk in yi:(yj-1)
          push!(zs, construct2(Z, zkey1, zkey2, xs[xk], ys[yk]))
        end
      end
      xi = xj
      yi = yj
    end
  end
  zs
end

@inline join_sorted(xs, ys, ztype, xkey, ykey, zkey1, zkey2) =
  join_sorted_inner(xs, ys, ztype, Val{xkey}, Val{ykey}, Val{zkey1}, Val{zkey2})

semijoin_sorted_inner{X,Y,XK,YK}(
  xs::Vector{X}, ys::Vector{Y},
  xkey::Type{Val{XK}}, ykey::Type{Val{YK}}
  ) = begin
  zs = Vector{X}(0)
  xi = 1
  yi = 1
  while (xi <= length(xs)) && (yi <= length(ys))
    x = xs[xi]
    y = ys[yi]
    c = cmp_by_key(x, y, xkey, ykey)
    if c == -1
      xi += 1
    elseif c == 1
      yi += 1
    else
      push!(zs, x)
      xi += 1
    end
  end
  zs
end

@inline semijoin_sorted(xs, ys, xkey, ykey) =
  semijoin_sorted_inner(xs, ys, Val{xkey}, Val{ykey})
```

And import the data for the benchmark:

``` julia
read_tsv(rowtype, filename) = begin
  fieldtypes = [fieldtype(rowtype, fieldname) for fieldname in fieldnames(rowtype)]
  raw = readdlm(filename, '\t', UTF8String, header=true, quotes=false, comments=false)[1]
  results = Vector{rowtype}(0)
  for i in 1:size(raw,1)
    row = Vector{Any}(vec(raw[i,:]))
    for j in 1:length(fieldtypes)
      if issubtype(fieldtypes[j], Number)
        row[j] = parse(fieldtypes[j], row[j])
      end
    end
    push!(results, rowtype(row...))
  end
  results
end

@row(Artist, [Int64, UTF8String])
@row(Album, [Int64, UTF8String, Int64])
@row(Track, [Int64, UTF8String, Int64, Int64, Int64, UTF8String, Float64, Float64, Float64])
@row(PlaylistTrack, [Int64, Int64])
@row(Playlist, [Int64, UTF8String])

chinook() = begin
  Any[
    read_tsv(Artist, "code/imp/data/Artist.csv"),
    read_tsv(Album, "code/imp/data/Album.csv"),
    read_tsv(Track, "code/imp/data/Track.csv"),
    read_tsv(PlaylistTrack, "code/imp/data/PlaylistTrack.csv"),
    read_tsv(Playlist, "code/imp/data/Playlist.csv"),
    ]
end
```

And finally, hand-compile the query plan:

``` julia
@row(I1, [Int64, UTF8String]) # playlist_id playlist_name
@row(I2, [Int64, Int64]) # playlist_id track_id
@row(I3, [Int64, Int64]) # track_id playlist_id
@row(I4, [Int64, Int64]) # track_id album_id
@row(I5, [Int64, Int64]) # album_id track_id
@row(I6, [Int64, Int64]) # album_id artist_id
@row(I7, [Int64, Int64]) # artist_id album_id
@row(I8, [Int64, UTF8String]) # artist_id artist_name
@row(I9, [Int64, UTF8String]) # album_id artist_name
@row(I10, [Int64, UTF8String]) # track_id artist_name
@row(I11, [Int64, UTF8String]) # playlist_id artist_name
@row(I12, [UTF8String, UTF8String]) # playlist_name artist_name

metal(data) = begin
  i0 = filter(row -> row.f2 == "Heavy Metal Classic", data[5])

  i1 = project(i0, I1, (1,2))
  i2 = project(data[4], I2, (1,2))
  i2s = semijoin_sorted(i2::Vector{I2}, i1::Vector{I1}, (1,), (1,))

  i3 = project(i2s, I3, (2,1))
  i4 = project(data[3], I4, (1,3))
  i4s = semijoin_sorted(i4, i3, (1,), (1,))

  i5 = project(i4s, I5, (2,1))
  i6 = project(data[2], I6, (1,3))
  i6s = semijoin_sorted(i6, i5, (1,), (1,))

  i7 = project(i6s, I7, (2,1))
  i8 = project(data[1], I8, (1,2))
  i9 = join_sorted(i7, i8, I9, (1,), (1,), (2,), (2,))

  i9s = project(i9, I9, (1,2))
  i10 = join_sorted(i5, i9s, I10, (1,), (1,), (2,), (2,))

  i10s = project(i10, I10, (1,2))
  i11 = join_sorted(i3, i10s, I11, (1,), (1,), (2,), (2,))

  i11s = project(i11, I11, (1,2))
  i12 = join_sorted(i1, i11s, I12, (1,), (1,), (2,), (2,))

  i12
end
```

Fingers crossed:

``` julia
using Benchmark

f() = begin
  data = chinook()
  @time benchmark(()->metal(data), "", 1000)
end

# mean 1.57ms, max 6.68ms, min 1.27ms
# total 3.354096 seconds (1.70 M allocations: 1.349 GB, 4.40% gc time)
```

Compared to the original:

``` bash
jamie@wanderer:~/code/imp$ cargo bench bench_chinook
     Running target/release/imp-fdad7c291f20f4c3

running 1 test
test runtime::tests::bench_chinook_metal     ... bench:   1,524,175 ns/iter (+/- 188,483)
```

Eerily neck and neck. While the Julia stdlib sort is slower than the radix sort used in Rust imp, it catches up by making reshape and join much faster.

I'm really happy with this. The code is easy to write and debug. Being able to freely mix generated code with normal functions is basically a superpower. It's funny that the two features that made it possible were quietly added without much fanfare. Is Julia the only commercially supported language for multi-stage programming? I can only think of [Terra](http://terralang.org/), [MetaOCaml](http://www.cs.rice.edu/~taha/MetaOCaml/) and [LMS](https://scala-lms.github.io/), none of which I'd want to risk using right now.
