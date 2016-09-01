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

## Indexing

I put together a crude first pass at a HAMT-like thing.

``` julia
type Node{T}
  leaf_bitmap::UInt32
  node_bitmap::UInt32
  leaves::Vector{T}
  nodes::Vector{Node{T}}
end

type Tree{T}
  root::Node{T}
end

Tree(T) = Tree(Node{T}(0, 0, T[], Node{T}[]))

const key_length = Int64(ceil(sizeof(hash(0)) * 8.0 / 5.0))

chunk_at(key, ix) = (key >> (ix*5)) & 0b11111

singleton{T}(row::T, column, ix) = begin
  if ix >= key_length
    column += 1
    ix = 0
    if column > length(row)
      error("Out of bits")
    end
  end
  value = row[column]
  key = hash(value)
  chunk = chunk_at(key, ix)
  Node{T}(1 << chunk, 0, T[row], Node{T}[])
end

Base.push!{T}(tree::Tree{T}, row::T) = begin
  node = tree.root
  for column in 1:length(row)
    value = row[column]
    key = hash(value)
    for ix in 0:(key_length-1)
      chunk = chunk_at(key, ix)
      mask = 1 << chunk
      if (node.node_bitmap & mask) > 0
        node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
        node = node.nodes[node_ix]
        # continue loop
      elseif (node.leaf_bitmap & mask) > 0
        leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
        leaf = node.leaves[leaf_ix]
        if row == leaf
          return tree # was a dupe
        else
          deleteat!(node.leaves, leaf_ix)
          child = singleton(leaf, column, ix+1)
          node.leaf_bitmap $= mask
          node.node_bitmap |= mask
          node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
          insert!(node.nodes, node_ix, child)
          node = child
          # continue loop
        end
      else
        node.leaf_bitmap |= mask
        leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
        insert!(node.leaves, leaf_ix, row)
        return tree # inserted
      end
    end
  end
  error("Out of bits!")
end

Base.in{T}(row::T, tree::Tree{T}) = begin
  node = tree.root
  for column in 1:length(row)
    value = row[column]
    key = hash(value)
    for ix in 0:(key_length-1)
      chunk = chunk_at(key, ix)
      mask = 1 << chunk
      if (node.node_bitmap & mask) > 0
        node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
        node = node.nodes[node_ix]
        # continue loop
      elseif (node.leaf_bitmap & mask) > 0
        leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
        leaf = node.leaves[leaf_ix]
        return row == leaf
      else
        return false
      end
    end
  end
  error("Out of bits!")
end
```

Naively comparing it to sorting to get a feel for how far away it is from the baseline:

``` julia
f() = begin
  rows = [(a,) for a in ids(1000000)]
  make_tree() = begin
    tree = Tree(Tuple{Int64})
    for row in rows
      push!(tree, row)
    end
    tree
  end
  tree = make_tree()
  (benchmark(()->sort(rows, alg=QuickSort), "", 100),
  benchmark(make_tree, "", 100),
  benchmark(()->all([(row in tree) for row in rows]), "", 100))
end

# ~280ms for sorting
# ~890ms for insert
# ~480ms for lookup
```

So this first pass is about 4x slower to build an index than sorting is. Let's start speeding that up.

Some quick digging in the generated code reveals:

```
julia> @code_native count_ones(0)
    .text
Filename: int.jl
Source line: 133
    pushq    %rbp
    movq    %rsp, %rbp
Source line: 133
    movq    %rdi, %rax
    shrq    %rax
    movabsq    $6148914691236517205, %rcx # imm = 0x5555555555555555
    andq    %rax, %rcx
    subq    %rcx, %rdi
    movabsq    $3689348814741910323, %rcx # imm = 0x3333333333333333
    movq    %rdi, %rax
    andq    %rcx, %rax
    shrq    $2, %rdi
    andq    %rcx, %rdi
    addq    %rax, %rdi
    movabsq    $72340172838076673, %rcx # imm = 0x101010101010101
    movabsq    $1085102592571150095, %rax # imm = 0xF0F0F0F0F0F0F0F
    movq    %rdi, %rdx
    shrq    $4, %rdx
    addq    %rdi, %rdx
    andq    %rdx, %rax
    imulq    %rcx, %rax
    shrq    $56, %rax
    popq    %rbp
    ret
```

Which is not what I was hoping. Googling avails me not. Kristoffer Carlsson [points me in the right direction](https://groups.google.com/forum/#!topic/julia-users/z7To2f1i1K8). After rebuilding Julia for my native arch I get:

```
julia> @code_native count_ones(0)
	.text
Filename: int.jl
Source line: 133
	pushq	%rbp
	movq	%rsp, %rbp
Source line: 133
	popcntq	%rdi, %rax
	popq	%rbp
	ret
```

Rebuilding nets us a small improvement in both sorting and insert:

```
# ~170ms for sorting
# ~560ms for insert
# ~500ms for lookup
```

I *think* the slight increase in lookup is just crappy benchmarking on my part, but I don't have a good way to reset the system image to test it. And I have no idea where to start looking if I want to figure out why sorting is faster. Maybe if I could find out what extra instructions were enabled...

Since sorting is a little faster now I reran the query benchmark from the last post and got ~1.4ms, down from ~1.6ms, making it just slightly faster on average than the Rust version.

Next I made `Node{T}` immutable, thinking that it would be stored inline in the parent array, saving half of the pointer chases. No change in the benchmarks. Uh oh.

``` julia
immutable A
  a::UInt32
  b::UInt32
  c::UInt64
  d::UInt64
end

sizeof(A[A(0,0,0,0), A(0,0,0,0)]) # 48

immutable B
  a::UInt32
  b::UInt32
  c::Vector{UInt64}
  d::Vector{UInt64}
end

sizeof(B[B(0,0,[],[]), B(0,0,[],[])]) # 16
```

At first glance this kind of makes sense, because if `Vector{UInt64}` was just a pointer to some contiguous array then that pointer would have to be mutated if the array is resized. (In Rust that's fine - we can pass around interior pointers and happily mutate them.)

But thinking about it some more, the same is true of variables on the stack. If I do:

``` julia
a = [1,2,3]
b = a
push!(a,4)
println(b)
```

The only way this could work is if a and b both point to some heap value which itself points to the actual array data. Looking at https://github.com/JuliaLang/julia/blob/master/src/julia.h#L193-L229 confirms my suspicion - we have a double pointer hop for every use of an array.

That doesn't explain why `B` doesn't get stored inline in the array, but it kicks a hole in this whole plan anyway. I need to figure out how to get this down to a single pointer hop per node. Perhaps something like:

``` julia
type Node{T, L, N}
  leaf_bitmap::UInt32
  node_bitmap::UInt32
  leaves::NTuple{L, Nullable{T}}
  nodes::NTuple{N, Nullable{Any}}
end
```

It depends on where those numbers get stored. I have a sneaking suspicion that it's going to be in another boxed intermediate separating the nodes.

``` julia
sizeof(Node{Int, 0, 0}) # 8
```

It certainly doesn't seem to be in the node itself, unless `sizeof` is not counting the metadata.

Humbug.

## Layout

My [question about storing B inline](https://groups.google.com/forum/#!topic/julia-users/9ADnjy1Zcx4) was answered - only types which satisfiy `isbits` will be stored inline ie no types that contain pointers. Fixing that would require modifying the gc, so it's very low on my list of options :)

My plan for today is to better understand the Julia runtime, starting with memory layout.

Tagged Julia values are defined in [julia.h](https://github.com/JuliaLang/julia/blob/master/src/julia.h#L151-L158) - a pointer to the type (with the lower 2 bits used for gc) followed by the value itself. The types are also tagged Julia values, built out of structs later on in [julia.h](https://github.com/JuliaLang/julia/blob/master/src/julia.h#L298-L368). The recursion ends with
some hardcoded types that are initialized in [jltypes.c](https://github.com/JuliaLang/julia/blob/25c3659d6cec2ebf6e6c7d16b03adac76a47b42a/src/jltypes.c#L3177-L3611).

We can do horrible things with pointers to get a look at the underlying memory:

``` julia
julia> view_raw(x, n) = begin
         p = convert(Ptr{UInt}, pointer_from_objref(x))
         for e in pointer_to_array(p, (n,), false)
           println(e)
         end
       end
view_raw (generic function with 1 method)

julia> view_raw(Z(5, true, [1,2,3]), 3)
5
1
140234783861264
```

It looks like type constructors are cached, which makes sense because somewhere there has to be a method cache where these are the keys:

``` julia
julia> type X{T} a::T end

julia> x1 = X{Int}
X{Int64}

julia> x2 = X{Int}
X{Int64}

julia> is(x1, x2)
true

julia> pointer_from_objref(x1)
Ptr{Void} @0x00007f8af3e2f2b0

julia> pointer_from_objref(x2)
Ptr{Void} @0x00007f8af3e2f2b0
```

Arrays are defined in [julia.h](https://github.com/JuliaLang/julia/blob/master/src/julia.h#L219-L255). It's way more complicated than I expected so I need to poke around to see what's going on. Luckily, they seem to be allocated near other stuff so I don't segfault all other the place while guessing the length.

``` julia
julia> view_raw([], 10)
139805903543424
0
569348
0
0
0
0
139805834444816
139805834490240
0

julia> view_raw([7,7,7], 10)
139805896193984
3
561156
3
3
4294967296
7
7
7
0

julia> view_raw(collect(777:7777), 10)
28339056
7001
561158
7001
7001
0
0
139805835290192
139805901526672
10

julia> view_raw(collect(777:7777), 20)
28395088
7001
561158
7001
7001
0
0
139805835290192
139805901532816
20
544772
20
20
0
0
139805834283184
139805901532992
0
569348
0

julia> view_raw(collect(777:888), 20)
139805873991552
112
561156
112
112
0
777
778
779
780
781
782
783
784
785
786
787
788
789
790
```

Up to around 2048 words and it gets stored inline. Above that and it gets dumped somewhere else. (Which is weird, because [MALLOC_THRESH=1048576](https://github.com/JuliaLang/julia/blob/ea952fc289a8f8e2aeb317e0ccb9ce59ec745c4f/src/array.c#L555)).

Resizing is interesting:

``` julia
julia> x = collect(1:10)
4-element Array{Int64,1}:
...

julia> view_raw(x, 20)
139805874711584
10
561156
10
10
139814526530144
1
2
3
4
5
6
7
8
9
10
0
139805834285488
139805874711728
0

julia> Int64(pointer(x))
139805874711584

julia> append!(x, 1:10000000)
10000004-element Array{Int64,1}:
...

julia> view_raw(x, 20)
139805488328720
10000004
561158
10000004
16777216
4294967296
1
2
3
4
0
139805875018024
139805875017984
24
102404
24
24
1099511627776
7954884599197543732
8897249683018162292

julia> x = collect(1:2000)
2000-element Array{Int64,1}:
...

julia> view_raw(x, 20)
37207568
2000
557060
2000
2000
0
1
2
3
4
5
6
7
8
9
10
11
12
13
14

julia> append!(x, 1:10000000)
10002000-element Array{Int64,1}:
...

julia> view_raw(x, 20)
139805082062864
10002000
557062
10002000
16384000
0
1
2
3
4
5
6
7
8
9
10
11
12
13
14
```

If pushing causes the array to be resized it calls realloc and just sets the pointer. If the realloc resized it in place everything is fine. If the realloc made a new allocation elsewhere then we now have an extra pointer hop and, as far as I can tell from reading https://github.com/JuliaLang/julia/blob/master/src/array.c#L561-L607, the old allocation just hangs around. But that can only happen when the original allocation was inline which means the wasted allocation is fairly small.

There are also 6 words of overhead per (1d) array, not including the type tag. But if I count up fields in the struct I get 5 words and I've only ever seen the last word be 0. Maybe the list of dimensions is 0-terminated?

Anyway, I should definitely not be using arrays for my trie.

Other types are created in [boot.jl](https://github.com/JuliaLang/julia/blob/master/base/boot.jl). Strings are arrays of bytes, so they also pay the 6 word overhead. There don't seem to be any other surprises.

I'm tentatively assuming that tuples get laid out like C structs but I haven't found their constructor yet.

(I also got a [walkthrough](https://groups.google.com/forum/#!topic/julia-users/-qgRbw8AaaU) on how to get LLDB working nicely today, and spent a few hours trying to do the same for oprofile and valgrind with no success.)

(I also wrote the [beginnings of a layout inspector](https://github.com/jamii/imp/blob/master/src/Layout.jl) which I have vague plans of turning into a little gui thing.)

## Baseline

How much is my naive julia layout costing me?

I [ported the Julia HAMT to Rust](https://github.com/jamii/imp/blob/7c3d76afe7c2288c13677966ff28a870c8b7ea85/src/map.rs). The code is nearly identical. Since they use different hashing algorithms I removed hashing in both and instead just use random keys generated uniformly over the whole UInt64 range. The Julia version *should* have full type information and obviously the Rust version is fully typed. The both use the same codegen and are both specializing on type.

```
Rust sort 1M - 0.08s

Rust insert 1M - 0.21s
Rust lookup 1M - 0.12s
Rust insert 10M - 3.11s
Rust lookup 10M - 1.92s
Rust insert 100M - 48.97s
Rust lookup 100M - 31.26s

Julia insert 1M - 0.59s
Julia lookup 1M - 0.25s
Julia insert 10M - 8.59s
Julia lookup 10M - 5.02s
Julia insert 100M - OOM
Julia lookup 100M - OOM

Rust insert 10M peak rss - 475MB
Julia insert 10M peak rss - 1034MB
Julia insert 10M allocations - 765MB
```

(Note that the Rust version could be further optimized. It is currently storing the lengths and capacity of both vectors in each node - an extra 32 bytes per node that could be calculated from the bitmaps instead.)

I suspect that the difference in performance is attributable mostly to the different memory layout (and, for insert, the cost of allocation/gc). To test this, I'm going to add the same overhead to the Rust version:

``` rust
#[derive(Clone, Debug)]
pub struct JuliaArray<T> {
    metadata: [u64; 5], // 7 words, but length/capactity are shared with Vector
    vec: Vec<T>,
}

#[derive(Clone, Debug)]
pub struct Node<T> {
    metadata: [u64; 1],
    leaf_bitmap: u32,
    node_bitmap: u32,
    leaves: Box<JuliaArray<T>>,
    nodes: Box<JuliaArray<Node<T>>>,
}
```

```
Julian Rust insert 1M - 0.29s
Julian Rust lookup 1M - 0.18s
Julian Rust insert 10M - 4.91s
Julia Rust lookup 10M - 3.28s

Julian Rust insert 10M peak rss - 670MB
```

Huh. The memory usage is now similar but the Rust version is still much faster.

The Julia insert spends ~20% of it's time in gc, which would still only bring the Rust version up to ~6s vs ~9s for Julia. The lookup doesn't report any allocations in Julia, so I think I can rule out the cost of allocation.

So now I'm not sure what's going on. Let's try looking at the cpu performance counters. I [can't get line numbers for Julia](https://groups.google.com/forum/#!topic/julia-users/-qgRbw8AaaU) but I can still see if there is any difference in the overall pattern between the two.

No, wait, before that - I forgot an extra overhead from Julia. Nodes are boxed too:

``` rust
#[derive(Clone, Debug)]
pub struct JuliaArray<T> {
    metadata: [u64; 5], // 7 words, but length/capactity are shared with Vector
    vec: Vec<T>,
}

#[derive(Clone, Debug)]
pub struct Node<T> {
    metadata: [u64; 1],
    leaf_bitmap: u32,
    node_bitmap: u32,
    leaves: Box<JuliaArray<T>>,
    nodes: Box<JuliaArray<Box<Node<T>>>>,
}
```

```
Julian Rust insert 1M - 0.36s
Julian Rust lookup 1M - 0.24s
Julian Rust insert 10M - 6.00
Julian Rust lookup 10M - 4.83s

Julia Rust insert 10M peak rss - 633MB
```

So now the lookup time is almost identical. The insert time is still short even if we add 20% gc. Let's break out the perf counters! These numbers are for 10x insert+lookup 10M rows:

```
jamie@wanderer:~/code/imp/src$ perf stat -e cycles,instructions,branches,branch-misses,context-switches,cache-references,cache-misses -B ../target/release/imp
5.199791108999989s for insert
4.474710060000007s for lookup
7.080400916000144s for insert
6.229417960999854s for lookup
7.060942442999931s for insert
6.299797809999973s for lookup
7.109493033000035s for insert
6.253823409000006s for lookup
7.016020142000116s for insert
6.215963810000176s for lookup
7.078056773000071s for insert
6.315231796000035s for lookup
7.38005259800002s for insert
6.26794608199998s for lookup
7.138151556999901s for insert
6.276204322000012s for lookup
7.058818885999926s for insert
6.217261228000098s for lookup
7.040932895000196s for insert
6.261231286000111s for lookup

 Performance counter stats for '../target/release/imp':

   356,295,147,143      cycles                                                        (66.67%)
   138,966,714,020      instructions              #    0.39  insns per cycle          (83.33%)
    19,330,421,477      branches                                                      (83.33%)
       335,132,522      branch-misses             #    1.73% of all branches          (83.33%)
               407      context-switches                                            
     3,865,389,150      cache-references                                              (83.33%)
     2,242,127,883      cache-misses              #   58.005 % of all cache refs      (83.33%)

     159.602175650 seconds time elapsed
```

The Rust version is faster on the first iteration and slower for all following iterations. This is consistent across multiple runs. I have no idea why. But it means that my benchmarks earlier with a single run per process are not telling the whole story. That's what I get for being lazy.

```
jamie@wanderer:~/code/imp/src$ perf stat -e cycles,instructions,branches,branch-misses,context-switches,cache-references,cache-misses -B julia Hamt.jl
WARNING: Base.String is deprecated, use AbstractString instead.
  likely near /home/jamie/.julia/v0.4/Benchmark/src/benchmarks.jl:13
WARNING: Base.String is deprecated, use AbstractString instead.
  likely near /home/jamie/.julia/v0.4/Benchmark/src/benchmarks.jl:13
WARNING: Base.String is deprecated, use AbstractString instead.
  likely near /home/jamie/.julia/v0.4/Benchmark/src/benchmarks.jl:41

WARNING: deprecated syntax "{a,b, ...}" at /home/jamie/.julia/v0.4/Benchmark/src/compare.jl:23.
Use "Any[a,b, ...]" instead.
  8.136648 seconds (15.31 M allocations: 766.329 MB, 10.92% gc time)
  4.815498 seconds (2 allocations: 9.537 MB)

 10.014619 seconds (15.31 M allocations: 766.329 MB, 25.24% gc time)
  4.692582 seconds (2 allocations: 9.537 MB)

 10.211293 seconds (15.31 M allocations: 766.329 MB, 23.99% gc time)
  5.035553 seconds (2 allocations: 9.537 MB)

  9.908472 seconds (15.31 M allocations: 766.329 MB, 24.53% gc time)
  4.786145 seconds (2 allocations: 9.537 MB)

 10.174752 seconds (15.31 M allocations: 766.329 MB, 24.43% gc time)
  4.983574 seconds (2 allocations: 9.537 MB)

  9.897518 seconds (15.31 M allocations: 766.329 MB, 24.45% gc time)
  4.976863 seconds (2 allocations: 9.537 MB)

 10.016775 seconds (15.31 M allocations: 766.329 MB, 24.93% gc time)
  4.608078 seconds (2 allocations: 9.537 MB)

  9.571723 seconds (15.31 M allocations: 766.329 MB, 24.56% gc time)
  4.663033 seconds (2 allocations: 9.537 MB)

  9.683189 seconds (15.31 M allocations: 766.329 MB, 24.38% gc time)
  4.614693 seconds (2 allocations: 9.537 MB)

 10.616815 seconds (15.31 M allocations: 766.329 MB, 23.37% gc time)
  5.474383 seconds (2 allocations: 9.537 MB)

nothing

 Performance counter stats for 'julia Hamt.jl':

   461,891,446,139      cycles                                                        (66.66%)
   172,704,455,804      instructions              #    0.37  insns per cycle          (83.33%)
    30,037,830,863      branches                                                      (83.34%)
       448,233,381      branch-misses             #    1.49% of all branches          (83.34%)
            18,070      context-switches                                            
     5,017,726,390      cache-references                                              (83.34%)
     2,915,696,296      cache-misses              #   58.108 % of all cache refs      (83.33%)

     169.362873754 seconds time elapsed
```

Same effect in the Julia version. Benchmarking is hard :S

What else can we see. The Julia version runs for longer, executes more instructions, executes more branches and reads memory more often. The branch-miss rate and cache-miss rate are very similar to the Rust version.

But the Julia version has waaaay more context-switches: 18,070 vs 407. Where are they coming from?

```
jamie@wanderer:~/code/imp/src$ sudo perf record -e context-switches --call-graph dwarf -B julia Hamt.jl
...
jamie@wanderer:~/code/imp/src$ sudo perf report -g graph --no-children

-   99.57%  julia          [kernel.kallsyms]  [k] schedule                                           
   - schedule                                                                                        
      - 70.90% retint_careful                                                                        
           9.08% 0x7f80f5a386bc                                                                      
           9.03% 0x7f80f4aa715f                                                                      
           7.37% 0x7f80f4aa7d85                                                                      
           6.09% 0x7f80f4aa8964                                                                      
           3.19% 0x7f80f5a383c5                                                                      
           2.85% 0x7f80f5a38a8d                                                                      
           2.47% 0x7f80f5a387fb                                                                      
           2.47% 0x7f80f5a383fa                                                                      
           2.23% 0x7f80f5a387f4                                                                      
           2.23% 0x7f80f5a383f3   
    etc...        
```

Gee, thanks perf.

```
jamie@wanderer:~/code/imp/src$ strace -c julia Hamt.jl
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
 87.15    0.008217          74       111           read
 12.74    0.001201           0    206532           madvise
  0.12    0.000011           0       116           mmap
...
```

The only place that madvise is called in the Julia repo is in [gc.c](https://github.com/JuliaLang/julia/blob/6cc48dcd24322976bdc193b3c578acb924f0b8e9/src/gc.c#L952). It looks like something to do with the allocation pools that Julia uses for small allocations. If I disable the gc I still get lots of context switches but no madvise. In fact, without gc I have ~12k context switches and only ~4k system calls. Mysterious.

Let's try something different. I'll start up Julia, compile everything and run through the benchmark once, then attach perf and run through it again. Just to see if those context switches are actually coming from my code or are all from compilation.

```
jamie@wanderer:~$ perf stat -e cycles,instructions,branches,branch-misses,context-switches,cache-references,cache-misses -p 6315
^C
 Performance counter stats for process id '6315':

   363,276,940,351      cycles                                                        (66.67%)
   163,202,516,794      instructions              #    0.45  insns per cycle          (83.33%)
    28,053,778,030      branches                                                      (83.33%)
       418,751,631      branch-misses             #    1.49% of all branches          (83.33%)
               848      context-switches                                            
     4,859,874,620      cache-references                                              (83.33%)
     2,563,913,645      cache-misses              #   52.757 % of all cache refs      (83.33%)

     173.421545858 seconds time elapsed
```

So it was a red herring. Bad benchmarking again :S

But we can see now that the numbers are very similar, with the Julia version just doing more work overall and nothing in particular standing out.

If we run without gc...

```
julia> f()
  5.709961 seconds (15.30 M allocations: 766.156 MB)
  3.705573 seconds (2 allocations: 9.537 MB)

  6.067698 seconds (15.30 M allocations: 766.156 MB)
  3.800082 seconds (2 allocations: 9.537 MB)

  5.877812 seconds (15.30 M allocations: 766.156 MB)
  3.573286 seconds (2 allocations: 9.537 MB)

  5.663457 seconds (15.30 M allocations: 766.156 MB)
  3.539182 seconds (2 allocations: 9.537 MB)

  5.710040 seconds (15.30 M allocations: 766.156 MB)
  3.711398 seconds (2 allocations: 9.537 MB)

  6.102098 seconds (15.30 M allocations: 766.156 MB)
  3.679805 seconds (2 allocations: 9.537 MB)

  6.603569 seconds (15.30 M allocations: 766.156 MB)
  3.722283 seconds (2 allocations: 9.537 MB)

Killed
```

..it's actually faster than the Rust version. It looks like the Rust version isn't generating popcnt. Cargo won't let me pass options to LLVM so we have this monstrousity instead:

```
jamie@wanderer:~/code/imp$ rustc src/main.rs --crate-name imp --crate-type bin -C opt-level=3 -g --out-dir /home/jamie/code/imp/target/release --emit=dep-info,link -L dependency=/home/jamie/code/imp/target/release -L dependency=/home/jamie/code/imp/target/release/deps --extern time=/home/jamie/code/imp/target/release/deps/libtime-22c21fe32894ddad.rlib --extern regex=/home/jamie/code/imp/target/release/deps/libregex-ca23fbfc498b741a.rlib --extern peg_syntax_ext=/home/jamie/code/imp/target/release/deps/libpeg_syntax_ext-3094fcce08564e8c.so --extern rand=/home/jamie/code/imp/target/release/deps/librand-12e778fcd5eb28e9.rlib -C target-cpu=native

jamie@wanderer:~/code/imp$ perf stat -e cycles,instructions,branches,branch-misses,context-switches,cache-references,cache-misses target/release/imp
3.5112703029999466s for insert
2.288155586999892s for lookup
5.355302826999832s for insert
2.946845485999802s for lookup
5.500407145000281s for insert
2.9404805889998897s for lookup
5.594997771000635s for insert
3.302149258999634s for lookup
5.507628701000613s for insert
3.064086700000189s for lookup
5.822303377000026s for insert
3.465523935999954s for lookup
5.836835606000022s for insert
3.4077694679999695s for lookup
6.276327544000196s for insert
3.469119299999875s for lookup
5.55367445399952s for insert
2.969313937000152s for lookup
5.593115818000115s for insert
3.053487084000153s for lookup

 Performance counter stats for 'target/release/imp':

   329,000,811,832      cycles                                                        (66.66%)
   120,350,860,733      instructions              #    0.37  insns per cycle          (83.33%)
    19,325,072,668      branches                                                      (83.33%)
       328,275,527      branch-misses             #    1.70% of all branches          (83.34%)
               855      context-switches                                            
     3,829,953,580      cache-references                                              (83.34%)
     2,075,840,379      cache-misses              #   54.200 % of all cache refs      (83.34%)

     111.281388975 seconds time elapsed
```

Sure makes a difference.

Finally, I realized that the Rust version isn't counting the time taken to free the tree after the benchmark, while the Julia version is paying that cost in gc during insert. So I'll time the Rust drop and the Julia gc separately after each run, and disable the gc otherwise.

```
julia> f()
  5.098385 seconds (15.30 M allocations: 765.923 MB)
  2.972040 seconds (2 allocations: 9.537 MB)
  1.975303 seconds, 100.00% gc time

  6.835982 seconds (15.30 M allocations: 765.923 MB)
  4.519367 seconds (2 allocations: 9.537 MB)
  2.255991 seconds, 100.00% gc time

  7.356767 seconds (15.30 M allocations: 765.923 MB)
  4.709045 seconds (2 allocations: 9.537 MB)
  2.694803 seconds, 100.00% gc time

  8.482212 seconds (15.30 M allocations: 765.923 MB)
  4.322303 seconds (2 allocations: 9.537 MB)
  2.330776 seconds, 100.00% gc time

  7.022945 seconds (15.30 M allocations: 765.923 MB)
  4.480007 seconds (2 allocations: 9.537 MB)
  2.261318 seconds, 100.00% gc time

  6.858507 seconds (15.30 M allocations: 765.923 MB)
  4.340702 seconds (2 allocations: 9.537 MB)
  2.283254 seconds, 100.00% gc time

  7.039293 seconds (15.30 M allocations: 765.923 MB)
  4.427812 seconds (2 allocations: 9.537 MB)
  2.270358 seconds, 100.00% gc time

  6.904196 seconds (15.30 M allocations: 765.923 MB)
  4.305911 seconds (2 allocations: 9.537 MB)
  2.345520 seconds, 100.00% gc time

  6.593265 seconds (15.30 M allocations: 765.923 MB)
  4.251790 seconds (2 allocations: 9.537 MB)
  2.277189 seconds, 100.00% gc time

  6.617482 seconds (15.30 M allocations: 765.923 MB)
  4.410888 seconds (2 allocations: 9.537 MB)
  2.478191 seconds, 100.00% gc time

jamie@wanderer:~/code/imp$ perf stat -e cycles,instructions,branches,branch-misses,context-switches,cache-references,cache-misses -p 8445
^C
 Performance counter stats for process id '8445':

   398,054,461,640      cycles                                                        (66.66%)
   132,453,050,158      instructions              #    0.33  insns per cycle          (83.33%)
    21,660,806,420      branches                                                      (83.33%)
       375,102,742      branch-misses             #    1.73% of all branches          (83.34%)
             1,341      context-switches                                            
     3,932,521,463      cache-references                                              (83.34%)
     2,337,178,854      cache-misses              #   59.432 % of all cache refs      (83.33%)
```

```
jamie@wanderer:~/code/imp$ perf stat -e cycles,instructions,branches,branch-misses,context-switches,cache-references,cache-misses target/release/imp
3.5180486560002464s for insert
2.8689307719996577s for lookup
2.254919785000311s for drop
6.634633460000259s for insert
3.2209488700000293s for lookup
2.691355521999867s for drop
5.714246575999823s for insert
3.3670170309997047s for lookup
2.601664489999166s for drop
6.162275493000379s for insert
4.827575713999977s for lookup
2.969592765000016s for drop
6.158259736000218s for insert
3.1756934750001165s for lookup
2.7672535319998133s for drop
5.674778747000346s for insert
3.1917649069991967s for lookup
2.652667846999975s for drop
5.5488982560000295s for insert
3.4133420640000622s for lookup
2.7529494509999495s for drop
5.82873814300001s for insert
3.2499250539995046s for lookup
2.7247443930000372s for drop
5.960908308000398s for insert
3.437930901999607s for lookup
2.88357682700007s for drop
5.899732896999922s for insert
3.095586663999711s for lookup
2.583231364999847s for drop

 Performance counter stats for 'target/release/imp':

   344,024,562,305      cycles                                                        (66.66%)
   120,094,681,100      instructions              #    0.35  insns per cycle          (83.32%)
    19,321,743,906      branches                                                      (83.33%)
       329,449,114      branch-misses             #    1.71% of all branches          (83.33%)
             1,915      context-switches                                            
     3,868,027,630      cache-references                                              (83.34%)
     2,160,131,618      cache-misses              #   55.846 % of all cache refs      (83.34%)
```

Julia is still slower. I have a version lying around that I compiled from source with debug symbols. Let's run that through the profiler and see if anything in the runtime looks suspicious.

```
jamie@wanderer:~/code/imp$ sudo perf record -p 10205
...
jamie@wanderer:~/code/imp$ sudo perf report
11.14%  julia    libjulia-debug.so  [.] gc_push_root
10.37%  julia    perf-10205.map     [.] 0x00007fd5ff08ba5c
10.03%  julia    perf-10205.map     [.] 0x00007fd5ff08b52d
 6.94%  julia    perf-10205.map     [.] 0x00007fd5ff08ba97
 6.62%  julia    perf-10205.map     [.] 0x00007fd5ff08b564
 4.77%  julia    perf-10205.map     [.] 0x00007fd5ff08b2fb
 4.53%  julia    perf-10205.map     [.] 0x00007fd5ff08ba90
 4.45%  julia    perf-10205.map     [.] 0x00007fd5ff08b55d
 3.14%  julia    libc-2.21.so       [.] __memmove_avx_unaligned
 3.04%  julia    libjulia-debug.so  [.] sweep_page
 2.95%  julia    libjulia-debug.so  [.] gc_setmark_pool
 2.93%  julia    libjulia-debug.so  [.] __pool_alloc
 2.81%  julia    libjulia-debug.so  [.] gc_setmark_buf
 1.85%  julia    libjulia-debug.so  [.] push_root
 1.78%  julia    perf-10205.map     [.] 0x00007fd5ff08bbcd
 1.71%  julia    perf-10205.map     [.] 0x00007fd5ff08bbc0
 1.39%  julia    perf-10205.map     [.] 0x00007fd5ff08b5cc
 1.34%  julia    libjulia-debug.so  [.] _new_array_
 1.32%  julia    perf-10205.map     [.] 0x00007fd5ff08b5c5
 0.97%  julia    perf-10205.map     [.] 0x00007fd5ff08adf6
 0.93%  julia    libc-2.21.so       [.] __memcpy_avx_unaligned
 0.74%  julia    libjulia-debug.so  [.] jl_array_grow_end
 0.58%  julia    libjulia-debug.so  [.] find_region
 0.55%  julia    perf-10205.map     [.] 0x00007fd5ff08b247
...
```

Not really. We already knew gc was about 20% of the cost. The hex values are jitted code ie not part of the runtime. Nothing else is that expensive.

Eugh, I forgot to remove one of the hashes in the Julia version. Julia's lookup is actually as slow as its insert, and 2x as slow as Rust's lookup. There must be something wrong in there somewhere.

Let's look at the ast for the lookup:

```
julia> @code_warntype in((UInt64(1),), tree)
Variables:
  row::Tuple{UInt64}
  tree::Tree{Tuple{UInt64}}
  node::Node{Tuple{UInt64}}
  #s4::Int64
  column::Int64
  value::UInt64
  key::UInt64
  #s1::Int64
  ix::Int64
  chunk::UInt64
  mask::Int64
  node_ix::Int64
  leaf_ix::Int64
  leaf::Tuple{UInt64}

Body:
  begin  # /home/jamie/code/imp/src/Hamt.jl, line 72: # /home/jamie/code/imp/src/Hamt.jl, line 73:
      node = (top(getfield))(tree::Tree{Tuple{UInt64}},:root)::Node{Tuple{UInt64}} # /home/jamie/code/imp/src/Hamt.jl, line 74:
      GenSym(4) = (Base.nfields)(row::Tuple{UInt64})::Int64
      GenSym(0) = $(Expr(:new, UnitRange{Int64}, 1, :(((top(getfield))(Base.Intrinsics,:select_value)::I)((Base.sle_int)(1,GenSym(4))::Bool,GenSym(4),(Base.box)(Int64,(Base.sub_int)(1,1)))::Int64)))
      #s4 = (top(getfield))(GenSym(0),:start)::Int64
      unless (Base.box)(Base.Bool,(Base.not_int)(#s4::Int64 === (Base.box)(Base.Int,(Base.add_int)((top(getfield))(GenSym(0),:stop)::Int64,1))::Bool)) goto 1
      2:
      GenSym(6) = #s4::Int64
      GenSym(7) = (Base.box)(Base.Int,(Base.add_int)(#s4::Int64,1))
      column = GenSym(6)
      #s4 = GenSym(7) # /home/jamie/code/imp/src/Hamt.jl, line 75:
      value = (Base.getfield)(row::Tuple{UInt64},column::Int64)::UInt64 # /home/jamie/code/imp/src/Hamt.jl, line 76:
      key = value::UInt64 # /home/jamie/code/imp/src/Hamt.jl, line 77:
      GenSym(5) = (Base.box)(Int64,(Base.sub_int)(Main.key_length,1))
      GenSym(2) = $(Expr(:new, UnitRange{Int64}, 0, :(((top(getfield))(Base.Intrinsics,:select_value)::I)((Base.sle_int)(0,GenSym(5))::Bool,GenSym(5),(Base.box)(Int64,(Base.sub_int)(0,1)))::Int64)))
      #s1 = (top(getfield))(GenSym(2),:start)::Int64
      unless (Base.box)(Base.Bool,(Base.not_int)(#s1::Int64 === (Base.box)(Base.Int,(Base.add_int)((top(getfield))(GenSym(2),:stop)::Int64,1))::Bool)) goto 5
      6:
      NewvarNode(:node_ix)
      NewvarNode(:leaf_ix)
      NewvarNode(:leaf)
      GenSym(8) = #s1::Int64
      GenSym(9) = (Base.box)(Base.Int,(Base.add_int)(#s1::Int64,1))
      ix = GenSym(8)
      #s1 = GenSym(9) # /home/jamie/code/imp/src/Hamt.jl, line 78:
      chunk = (Base.box)(UInt64,(Base.and_int)((Base.box)(UInt64,(Base.lshr_int)(key::UInt64,(Base.box)(Int64,(Base.mul_int)(ix::Int64,5)))),(Base.box)(UInt64,(Base.zext_int)(UInt64,0x1f)))) # /home/jamie/code/imp/src/Hamt.jl, line 79:
      mask = (Base.box)(Int64,(Base.shl_int)(1,chunk::UInt64)) # /home/jamie/code/imp/src/Hamt.jl, line 80:
      unless (Base.slt_int)(0,(Base.box)(Int64,(Base.and_int)((Base.box)(Int64,(Base.zext_int)(Int64,(top(getfield))(node::Node{Tuple{UInt64}},:node_bitmap)::UInt32)),mask::Int64)))::Bool goto 8 # /home/jamie/code/imp/src/Hamt.jl, line 81:
      node_ix = (Base.box)(Base.Int,(Base.add_int)(1,(Base.box)(Int64,(Base.zext_int)(Int64,(Base.box)(UInt32,(Base.ctpop_int)((Base.box)(UInt32,(Base.shl_int)((top(getfield))(node::Node{Tuple{UInt64}},:node_bitmap)::UInt32,(Base.box)(UInt64,(Base.sub_int)((Base.box)(UInt64,(Base.check_top_bit)(32)),chunk::UInt64)))))))))) # /home/jamie/code/imp/src/Hamt.jl, line 82:
      node = (Base.arrayref)((top(getfield))(node::Node{Tuple{UInt64}},:nodes)::Array{Node{Tuple{UInt64}},1},node_ix::Int64)::Node{Tuple{UInt64}}
      goto 10
      8:  # /home/jamie/code/imp/src/Hamt.jl, line 84:
      unless (Base.slt_int)(0,(Base.box)(Int64,(Base.and_int)((Base.box)(Int64,(Base.zext_int)(Int64,(top(getfield))(node::Node{Tuple{UInt64}},:leaf_bitmap)::UInt32)),mask::Int64)))::Bool goto 9 # /home/jamie/code/imp/src/Hamt.jl, line 85:
      leaf_ix = (Base.box)(Base.Int,(Base.add_int)(1,(Base.box)(Int64,(Base.zext_int)(Int64,(Base.box)(UInt32,(Base.ctpop_int)((Base.box)(UInt32,(Base.shl_int)((top(getfield))(node::Node{Tuple{UInt64}},:leaf_bitmap)::UInt32,(Base.box)(UInt64,(Base.sub_int)((Base.box)(UInt64,(Base.check_top_bit)(32)),chunk::UInt64)))))))))) # /home/jamie/code/imp/src/Hamt.jl, line 86:
      leaf = (Base.arrayref)((top(getfield))(node::Node{Tuple{UInt64}},:leaves)::Array{Tuple{UInt64},1},leaf_ix::Int64)::Tuple{UInt64} # /home/jamie/code/imp/src/Hamt.jl, line 87:
      return row::Tuple{UInt64} == leaf::Tuple{UInt64}::Bool
      goto 10
      9:  # /home/jamie/code/imp/src/Hamt.jl, line 89:
      return false
      10:
      7:
      unless (Base.box)(Base.Bool,(Base.not_int)((Base.box)(Base.Bool,(Base.not_int)(#s1::Int64 === (Base.box)(Base.Int,(Base.add_int)((top(getfield))(GenSym(2),:stop)::Int64,1))::Bool)))) goto 6
      5:
      4:
      3:
      unless (Base.box)(Base.Bool,(Base.not_int)((Base.box)(Base.Bool,(Base.not_int)(#s4::Int64 === (Base.box)(Base.Int,(Base.add_int)((top(getfield))(GenSym(0),:stop)::Int64,1))::Bool)))) goto 2
      1:
      0:  # /home/jamie/code/imp/src/Hamt.jl, line 93:
      return (Base.throw)(((top(getfield))((top(getfield))(Base.Main,:Base)::Any,:call)::Any)((top(getfield))((top(getfield))(Base.Main,:Base)::Any,:ErrorException)::Any,"Out of bits!")::Any)::Union{}
  end::Bool
```

All the types are correctly derived. There is a lot of boxing going on, but it disappears by the time we reach LLVM IR:

```
define i1 @julia_in_22401([1 x i64]*, %jl_value_t*) {
pass:
  %row = alloca [1 x i64], align 8
  %leaf = alloca [1 x i64], align 8
  %2 = alloca [6 x %jl_value_t*], align 8
  %.sub = getelementptr inbounds [6 x %jl_value_t*]* %2, i64 0, i64 0
  %3 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 2
  %4 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 3
  store %jl_value_t* inttoptr (i64 8 to %jl_value_t*), %jl_value_t** %.sub, align 8
  %5 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 1
  %6 = load %jl_value_t*** @jl_pgcstack, align 8
  %.c = bitcast %jl_value_t** %6 to %jl_value_t*
  store %jl_value_t* %.c, %jl_value_t** %5, align 8
  store %jl_value_t** %.sub, %jl_value_t*** @jl_pgcstack, align 8
  store %jl_value_t* null, %jl_value_t** %3, align 8
  store %jl_value_t* null, %jl_value_t** %4, align 8
  %7 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 4
  store %jl_value_t* null, %jl_value_t** %7, align 8
  %8 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 5
  store %jl_value_t* null, %jl_value_t** %8, align 8
  %9 = load [1 x i64]* %0, align 8
  store [1 x i64] %9, [1 x i64]* %row, align 8
  %10 = getelementptr inbounds %jl_value_t* %1, i64 0, i32 0
  %11 = load %jl_value_t** %10, align 8
  store %jl_value_t* %11, %jl_value_t** %3, align 8
  %12 = getelementptr [1 x i64]* %0, i64 0, i64 0
  %13 = load i64* %12, align 8
  br label %L2

L2:                                               ; preds = %pass5, %pass
  %14 = phi %jl_value_t* [ %11, %pass ], [ %45, %pass5 ]
  %"#s1.0" = phi i64 [ 0, %pass ], [ %48, %pass5 ]
  %15 = mul i64 %"#s1.0", 5
  %16 = ashr i64 %13, %15
  %17 = and i64 %16, 31
  %18 = shl i64 1, %17
  %19 = bitcast %jl_value_t* %14 to i8*
  %20 = getelementptr i8* %19, i64 4
  %21 = bitcast i8* %20 to i32*
  %22 = load i32* %21, align 4
  %23 = zext i32 %22 to i64
  %24 = and i64 %18, %23
  %25 = icmp eq i64 %24, 0
  br i1 %25, label %L6, label %if3

if3:                                              ; preds = %L2
  %26 = sub i64 32, %17
  %27 = trunc i64 %26 to i32
  %28 = shl i32 %22, %27
  %29 = icmp ugt i64 %26, 31
  %30 = select i1 %29, i32 0, i32 %28
  %31 = call i32 @llvm.ctpop.i32(i32 %30)
  %32 = zext i32 %31 to i64
  %33 = getelementptr inbounds %jl_value_t* %14, i64 2, i32 0
  %34 = load %jl_value_t** %33, align 8
  %35 = getelementptr inbounds %jl_value_t* %34, i64 1
  %36 = bitcast %jl_value_t* %35 to i64*
  %37 = load i64* %36, align 8
  %38 = icmp ult i64 %32, %37
  br i1 %38, label %idxend, label %oob

oob:                                              ; preds = %if3
  %39 = add i64 %32, 1
  %40 = alloca i64, align 8
  store i64 %39, i64* %40, align 8
  call void @jl_bounds_error_ints(%jl_value_t* %34, i64* %40, i64 1)
  unreachable

idxend:                                           ; preds = %if3
  %41 = bitcast %jl_value_t* %34 to i8**
  %42 = load i8** %41, align 8
  %43 = bitcast i8* %42 to %jl_value_t**
  %44 = getelementptr %jl_value_t** %43, i64 %32
  %45 = load %jl_value_t** %44, align 8
  %46 = icmp eq %jl_value_t* %45, null
  br i1 %46, label %fail4, label %pass5

fail4:                                            ; preds = %idxend
  %47 = load %jl_value_t** @jl_undefref_exception, align 8
  call void @jl_throw_with_superfluous_argument(%jl_value_t* %47, i32 82)
  unreachable

pass5:                                            ; preds = %idxend
  %48 = add i64 %"#s1.0", 1
  store %jl_value_t* %45, %jl_value_t** %3, align 8
  %49 = icmp eq i64 %"#s1.0", 12
  br i1 %49, label %L17, label %L2

L6:                                               ; preds = %L2
  %50 = bitcast %jl_value_t* %14 to i32*
  %51 = load i32* %50, align 16
  %52 = zext i32 %51 to i64
  %53 = and i64 %52, %18
  %54 = icmp eq i64 %53, 0
  br i1 %54, label %L11, label %if7

if7:                                              ; preds = %L6
  %55 = sub i64 32, %17
  %56 = trunc i64 %55 to i32
  %57 = shl i32 %51, %56
  %58 = icmp ugt i64 %55, 31
  %59 = select i1 %58, i32 0, i32 %57
  %60 = call i32 @llvm.ctpop.i32(i32 %59)
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %jl_value_t* %14, i64 1, i32 0
  %63 = load %jl_value_t** %62, align 8
  %64 = getelementptr inbounds %jl_value_t* %63, i64 1
  %65 = bitcast %jl_value_t* %64 to i64*
  %66 = load i64* %65, align 8
  %67 = icmp ult i64 %61, %66
  br i1 %67, label %idxend9, label %oob8

oob8:                                             ; preds = %if7
  %68 = add i64 %61, 1
  %69 = alloca i64, align 8
  store i64 %68, i64* %69, align 8
  call void @jl_bounds_error_ints(%jl_value_t* %63, i64* %69, i64 1)
  unreachable

idxend9:                                          ; preds = %if7
  %70 = bitcast %jl_value_t* %63 to i8**
  %71 = load i8** %70, align 8
  %72 = bitcast i8* %71 to [1 x i64]*
  %73 = getelementptr [1 x i64]* %72, i64 %61
  %74 = load [1 x i64]* %73, align 8
  store [1 x i64] %74, [1 x i64]* %leaf, align 8
  %75 = call i1 @"julia_==540"([1 x i64]* %row, [1 x i64]* %leaf)
  %76 = load %jl_value_t** %5, align 8
  %77 = getelementptr inbounds %jl_value_t* %76, i64 0, i32 0
  store %jl_value_t** %77, %jl_value_t*** @jl_pgcstack, align 8
  ret i1 %75

L11:                                              ; preds = %L6
  %78 = load %jl_value_t** %5, align 8
  %79 = getelementptr inbounds %jl_value_t* %78, i64 0, i32 0
  store %jl_value_t** %79, %jl_value_t*** @jl_pgcstack, align 8
  ret i1 false

L17:                                              ; preds = %pass5
  %80 = load %jl_value_t** inttoptr (i64 139773691732392 to %jl_value_t**), align 8
  store %jl_value_t* %80, %jl_value_t** %4, align 8
  store %jl_value_t* inttoptr (i64 139782383826328 to %jl_value_t*), %jl_value_t** %7, align 8
  %81 = call %jl_value_t* @jl_f_get_field(%jl_value_t* null, %jl_value_t** %4, i32 2)
  store %jl_value_t* %81, %jl_value_t** %4, align 8
  store %jl_value_t* inttoptr (i64 139782383820776 to %jl_value_t*), %jl_value_t** %7, align 8
  %82 = call %jl_value_t* @jl_f_get_field(%jl_value_t* null, %jl_value_t** %4, i32 2)
  store %jl_value_t* %82, %jl_value_t** %4, align 8
  %83 = load %jl_value_t** inttoptr (i64 139773691732392 to %jl_value_t**), align 8
  store %jl_value_t* %83, %jl_value_t** %7, align 8
  store %jl_value_t* inttoptr (i64 139782383826328 to %jl_value_t*), %jl_value_t** %8, align 8
  %84 = call %jl_value_t* @jl_f_get_field(%jl_value_t* null, %jl_value_t** %7, i32 2)
  store %jl_value_t* %84, %jl_value_t** %7, align 8
  store %jl_value_t* inttoptr (i64 139782383893576 to %jl_value_t*), %jl_value_t** %8, align 8
  %85 = call %jl_value_t* @jl_f_get_field(%jl_value_t* null, %jl_value_t** %7, i32 2)
  store %jl_value_t* %85, %jl_value_t** %7, align 8
  store %jl_value_t* inttoptr (i64 139773758179264 to %jl_value_t*), %jl_value_t** %8, align 8
  %86 = getelementptr inbounds %jl_value_t* %82, i64 -1, i32 0
  %87 = load %jl_value_t** %86, align 8
  %88 = ptrtoint %jl_value_t* %87 to i64
  %89 = and i64 %88, -16
  %90 = inttoptr i64 %89 to %jl_value_t*
  %91 = icmp eq %jl_value_t* %90, inttoptr (i64 139773691822656 to %jl_value_t*)
  br i1 %91, label %isf, label %notf

isf:                                              ; preds = %L17
  %92 = bitcast %jl_value_t* %82 to %jl_value_t* (%jl_value_t*, %jl_value_t**, i32)**
  %93 = load %jl_value_t* (%jl_value_t*, %jl_value_t**, i32)** %92, align 8
  %94 = call %jl_value_t* %93(%jl_value_t* %82, %jl_value_t** %7, i32 2)
  br label %fail18

notf:                                             ; preds = %L17
  %95 = call %jl_value_t* @jl_apply_generic(%jl_value_t* inttoptr (i64 139773716284272 to %jl_value_t*), %jl_value_t** %4, i32 3)
  br label %fail18

fail18:                                           ; preds = %notf, %isf
  %96 = phi %jl_value_t* [ %94, %isf ], [ %95, %notf ]
  call void @jl_throw_with_superfluous_argument(%jl_value_t* %96, i32 93)
  unreachable
}
```

That's a lot of code. I learned a lot more llvm commands today than I planned to.

First thing I notice:

```
julia> x = UInt32(0)
0x00000000

julia> @code_llvm (x << 32)

define i32 @"julia_<<_21947"(i32, i64) {
top:
  %2 = trunc i64 %1 to i32
  %3 = shl i32 %0, %2
  %4 = icmp ugt i64 %1, 31
  %5 = select i1 %4, i32 0, i32 %3
  ret i32 %5
}

julia> @code_native (x << 32)
	.text
Filename: int.jl
Source line: 109
	pushq	%rbp
	movq	%rsp, %rbp
Source line: 109
	movb	%sil, %cl
	shll	%cl, %edi
	xorl	%eax, %eax
	cmpq	$31, %rsi
	cmovbel	%edi, %eax
	popq	%rbp
	ret
```

There is some extra logic in [shl_int](https://github.com/JuliaLang/julia/blob/15cae8649392195e6c5cb5a31eac87b4b49b2a85/src/intrinsics.cpp#L1332-L1339) to handle the case where the shift amount is more than the number of bits. Compare this to js:

```
jamie@wanderer:~$ nodejs
> 42 << 64
42
```

That was a nasty surprise earlier this year - js just masks off all but the bottom 5 bits. [Apparently C is free to do this too](http://stackoverflow.com/questions/7401888/why-doesnt-left-bit-shift-for-32-bit-integers-work-as-expected-when-used).

```
%46 = icmp eq %jl_value_t* %45, null
br i1 %46, label %fail4, label %pass5

fail4:                                            ; preds = %idxend
%47 = load %jl_value_t** @jl_undefref_exception, align 8
call void @jl_throw_with_superfluous_argument(%jl_value_t* %47, i32 82)
unreachable
```

Null check on `node = node.nodes[node_ix]`. Can I put nulls in arrays?

```
julia> Vector(3)
3-element Array{Any,1}:
 #undef
 #undef
 #undef
```

Yep.

```
%row = alloca [1 x i64], align 8
%leaf = alloca [1 x i64], align 8
%2 = alloca [6 x %jl_value_t*], align 8
%.sub = getelementptr inbounds [6 x %jl_value_t*]* %2, i64 0, i64 0
%3 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 2
%4 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 3
store %jl_value_t* inttoptr (i64 8 to %jl_value_t*), %jl_value_t** %.sub, align 8
%5 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 1
%6 = load %jl_value_t*** @jl_pgcstack, align 8
%.c = bitcast %jl_value_t** %6 to %jl_value_t*
store %jl_value_t* %.c, %jl_value_t** %5, align 8
store %jl_value_t** %.sub, %jl_value_t*** @jl_pgcstack, align 8
store %jl_value_t* null, %jl_value_t** %3, align 8
store %jl_value_t* null, %jl_value_t** %4, align 8
%7 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 4
store %jl_value_t* null, %jl_value_t** %7, align 8
%8 = getelementptr [6 x %jl_value_t*]* %2, i64 0, i64 5
store %jl_value_t* null, %jl_value_t** %8, align 8
```

Embedding a linked list in the stack, I think? It allocates stack space for the row and leaf tuples, and then for 6 pointers. The first pointer is set to 8 (ie the size of this stack frame), the second pointer is set to jl_pgcstack and jl_pgcstack is set to point at the first pointer.

```
%75 = call i1 @"julia_==540"([1 x i64]* %row, [1 x i64]* %leaf)
```

`chunk_at` was inlined but `row == leaf` was not. Maybe llvm will decide to inline it later? Replacing it with `row[1] == leaf[1]` has little impact.

The IR for the Rust version (with debug info stripped) is:

```
define internal fastcc zeroext i1 @"_ZN3map15Tree$LT$u64$GT$8contains20h0b90de763b72ec39YnaE"(%"map::Tree<u64>"* noalias nocapture readonly dereferenceable(8), i64* noalias nonnull readonly, i64) unnamed_addr #9 {
entry-block:
  %3 = icmp eq i64 %2, 0, !dbg !50185
  br i1 %3, label %clean_ast_26544_25, label %match_case.lr.ph, !dbg !50187

match_case.lr.ph:                                 ; preds = %entry-block
  %4 = getelementptr inbounds %"map::Tree<u64>", %"map::Tree<u64>"* %0, i64 0, i32 0, !dbg !50173
  br label %"_ZN5slice36_$u5b$T$u5d$.ops..Index$LT$usize$GT$5index5index21h17911719332607156456E.exit", !dbg !50187

loop_body.loopexit:                               ; preds = %"_ZN3vec31Vec$LT$T$GT$.Index$LT$usize$GT$5index5index20h7209727245129996211E.exit"
  %.lcssa227 = phi %"map::Node<u64>"** [ %29, %"_ZN3vec31Vec$LT$T$GT$.Index$LT$usize$GT$5index5index20h7209727245129996211E.exit" ]
  %5 = icmp ult i64 %6, %2, !dbg !50185
  br i1 %5, label %"_ZN5slice36_$u5b$T$u5d$.ops..Index$LT$usize$GT$5index5index21h17911719332607156456E.exit", label %clean_ast_26544_25.loopexit, !dbg !50187

"_ZN5slice36_$u5b$T$u5d$.ops..Index$LT$usize$GT$5index5index21h17911719332607156456E.exit": ; preds = %loop_body.loopexit, %match_case.lr.ph
  %node.0167 = phi %"map::Node<u64>"** [ %4, %match_case.lr.ph ], [ %.lcssa227, %loop_body.loopexit ]
  %.sroa.0125.0..val.i145166 = phi i64 [ 0, %match_case.lr.ph ], [ %6, %loop_body.loopexit ]
  %6 = add nuw i64 %.sroa.0125.0..val.i145166, 1, !dbg !50191
  %7 = getelementptr inbounds i64, i64* %1, i64 %.sroa.0125.0..val.i145166, !dbg !50212
  %8 = load i64, i64* %7, align 8, !dbg !50213
  br label %match_case13, !dbg !50218

match_case13:                                     ; preds = %"_ZN5slice36_$u5b$T$u5d$.ops..Index$LT$usize$GT$5index5index21h17911719332607156456E.exit", %"_ZN3vec31Vec$LT$T$GT$.Index$LT$usize$GT$5index5index20h7209727245129996211E.exit"
  %node.1165 = phi %"map::Node<u64>"** [ %node.0167, %"_ZN5slice36_$u5b$T$u5d$.ops..Index$LT$usize$GT$5index5index21h17911719332607156456E.exit" ], [ %29, %"_ZN3vec31Vec$LT$T$GT$.Index$LT$usize$GT$5index5index20h7209727245129996211E.exit" ]
  %.sroa.0105.0..val.i.80141164 = phi i64 [ 0, %"_ZN5slice36_$u5b$T$u5d$.ops..Index$LT$usize$GT$5index5index21h17911719332607156456E.exit" ], [ %9, %"_ZN3vec31Vec$LT$T$GT$.Index$LT$usize$GT$5index5index20h7209727245129996211E.exit" ]
  %9 = add nuw nsw i64 %.sroa.0105.0..val.i.80141164, 1, !dbg !50222
  %10 = mul nuw nsw i64 %.sroa.0105.0..val.i.80141164, 5, !dbg !50229
  %11 = lshr i64 %8, %10, !dbg !50229
  %.tr.i = trunc i64 %11 to i32, !dbg !50229
  %12 = and i32 %.tr.i, 31, !dbg !50229
  %13 = shl i32 1, %12, !dbg !50231
  %14 = load %"map::Node<u64>"*, %"map::Node<u64>"** %node.1165, align 8, !dbg !50232, !nonnull !361
  %15 = getelementptr inbounds %"map::Node<u64>", %"map::Node<u64>"* %14, i64 0, i32 2, !dbg !50232
  %16 = load i32, i32* %15, align 4, !dbg !50232
  %17 = and i32 %16, %13, !dbg !50232
  %18 = icmp eq i32 %17, 0, !dbg !50232
  br i1 %18, label %else-block, label %then-block-921-, !dbg !50232

then-block-921-:                                  ; preds = %match_case13
  %19 = zext i32 %16 to i64, !dbg !50235
  %20 = sub nsw i32 32, %12, !dbg !50235
  %21 = zext i32 %20 to i64, !dbg !50235
  %22 = shl i64 %19, %21, !dbg !50235
  %23 = trunc i64 %22 to i32, !dbg !50235
  %24 = tail call i32 @llvm.ctpop.i32(i32 %23) #1, !dbg !50238
  %25 = zext i32 %24 to i64, !dbg !50239
  %26 = getelementptr inbounds %"map::Node<u64>", %"map::Node<u64>"* %14, i64 0, i32 4, !dbg !50240
  %27 = load %"map::JuliaArray<Box<map::Node<u64>>>"*, %"map::JuliaArray<Box<map::Node<u64>>>"** %26, align 8, !dbg !50240, !nonnull !361
  %.idx79 = getelementptr %"map::JuliaArray<Box<map::Node<u64>>>", %"map::JuliaArray<Box<map::Node<u64>>>"* %27, i64 0, i32 1, i32 1
  %.idx79.val = load i64, i64* %.idx79, align 8, !alias.scope !50242
  %28 = icmp ugt i64 %.idx79.val, %25, !dbg !50253
  br i1 %28, label %"_ZN3vec31Vec$LT$T$GT$.Index$LT$usize$GT$5index5index20h7209727245129996211E.exit", label %cond.i, !dbg !50240, !prof !47339

cond.i:                                           ; preds = %then-block-921-
  %.idx79.val.lcssa = phi i64 [ %.idx79.val, %then-block-921- ]
  %.lcssa224 = phi i64 [ %25, %then-block-921- ]
  tail call void @_ZN9panicking18panic_bounds_check20h2760eb7b4877ebd5RmKE({ %str_slice, i32 }* noalias nonnull readonly dereferenceable(24) @panic_bounds_check_loc12605, i64 %.lcssa224, i64 %.idx79.val.lcssa), !dbg !50253
  unreachable, !dbg !50253

"_ZN3vec31Vec$LT$T$GT$.Index$LT$usize$GT$5index5index20h7209727245129996211E.exit": ; preds = %then-block-921-
  %.idx78 = getelementptr %"map::JuliaArray<Box<map::Node<u64>>>", %"map::JuliaArray<Box<map::Node<u64>>>"* %27, i64 0, i32 1, i32 0, i32 0, i32 0, i32 0
  %.idx78.val = load %"map::Node<u64>"**, %"map::Node<u64>"*** %.idx78, align 8, !alias.scope !50254
  %29 = getelementptr inbounds %"map::Node<u64>"*, %"map::Node<u64>"** %.idx78.val, i64 %25, !dbg !50253
  %30 = icmp ult i64 %9, 13, !dbg !50259
  br i1 %30, label %match_case13, label %loop_body.loopexit, !dbg !50218

else-block:                                       ; preds = %match_case13
  %.lcssa221 = phi %"map::Node<u64>"* [ %14, %match_case13 ]
  %.lcssa218 = phi i32 [ %13, %match_case13 ]
  %.lcssa = phi i32 [ %12, %match_case13 ]
  %31 = getelementptr inbounds %"map::Node<u64>", %"map::Node<u64>"* %.lcssa221, i64 0, i32 1, !dbg !50261
  %32 = load i32, i32* %31, align 4, !dbg !50261
  %33 = and i32 %32, %.lcssa218, !dbg !50261
  %34 = icmp eq i32 %33, 0, !dbg !50261
  br i1 %34, label %clean_ast_897_, label %then-block-951-, !dbg !50261

then-block-951-:                                  ; preds = %else-block
  %35 = zext i32 %32 to i64, !dbg !50272
  %36 = sub nsw i32 32, %.lcssa, !dbg !50272
  %37 = zext i32 %36 to i64, !dbg !50272
  %38 = shl i64 %35, %37, !dbg !50272
  %39 = trunc i64 %38 to i32, !dbg !50272
  %40 = tail call i32 @llvm.ctpop.i32(i32 %39) #1, !dbg !50275
  %41 = zext i32 %40 to i64, !dbg !50276
  %42 = mul i64 %41, %2, !dbg !50276
  %43 = getelementptr inbounds %"map::Node<u64>", %"map::Node<u64>"* %.lcssa221, i64 0, i32 3, !dbg !50277
  %44 = load %"map::JuliaArray<u64>"*, %"map::JuliaArray<u64>"** %43, align 8, !dbg !50277, !nonnull !361
  %45 = add i64 %42, %2, !dbg !50277
  %.idx = getelementptr %"map::JuliaArray<u64>", %"map::JuliaArray<u64>"* %44, i64 0, i32 1, i32 0, i32 0, i32 0, i32 0
  %.idx.val = load i64*, i64** %.idx, align 8
  %.idx75 = getelementptr %"map::JuliaArray<u64>", %"map::JuliaArray<u64>"* %44, i64 0, i32 1, i32 1
  %.idx75.val = load i64, i64* %.idx75, align 8
  %46 = icmp ult i64 %45, %42, !dbg !50295
  br i1 %46, label %then-block-37390-.i.i, label %else-block.i.i, !dbg !50295

then-block-37390-.i.i:                            ; preds = %then-block-951-
  tail call void @_ZN5slice22slice_index_order_fail20h86e0cbc11bd0c115C8NE(i64 %42, i64 %45), !dbg !50296, !noalias !50298
  unreachable, !dbg !50296

else-block.i.i:                                   ; preds = %then-block-951-
  %47 = icmp ugt i64 %45, %.idx75.val, !dbg !50309
  br i1 %47, label %then-block-37404-.i.i, label %"_ZN3vec54Vec$LT$T$GT$.ops..Index$LT$ops..Range$LT$usize$GT$$GT$5index5index21h10954783785114391776E.exit", !dbg !50309

then-block-37404-.i.i:                            ; preds = %else-block.i.i
  tail call void @_ZN5slice20slice_index_len_fail20h0426121f8200b444C7NE(i64 %45, i64 %.idx75.val), !dbg !50316, !noalias !50298
  unreachable, !dbg !50316

"_ZN3vec54Vec$LT$T$GT$.ops..Index$LT$ops..Range$LT$usize$GT$$GT$5index5index21h10954783785114391776E.exit": ; preds = %else-block.i.i
  %48 = getelementptr inbounds i64, i64* %.idx.val, i64 %42, !dbg !50325
  br label %loop_body.i.i, !dbg !50343

loop_body.i.i:                                    ; preds = %"_ZN3vec54Vec$LT$T$GT$.ops..Index$LT$ops..Range$LT$usize$GT$$GT$5index5index21h10954783785114391776E.exit", %next.i.i
  %.sroa.037.0..val.i46.i.i = phi i64 [ %50, %next.i.i ], [ 0, %"_ZN3vec54Vec$LT$T$GT$.ops..Index$LT$ops..Range$LT$usize$GT$$GT$5index5index21h10954783785114391776E.exit" ], !dbg !50344
  %49 = icmp ult i64 %.sroa.037.0..val.i46.i.i, %2, !dbg !50347
  br i1 %49, label %next.i.i, label %clean_ast_897_.loopexit, !dbg !50349

next.i.i:                                         ; preds = %loop_body.i.i
  %50 = add i64 %.sroa.037.0..val.i46.i.i, 1, !dbg !50353
  %51 = getelementptr inbounds i64, i64* %1, i64 %.sroa.037.0..val.i46.i.i, !dbg !50374
  %52 = getelementptr inbounds i64, i64* %48, i64 %.sroa.037.0..val.i46.i.i, !dbg !50375
  %.val.i.i = load i64, i64* %51, align 8, !dbg !50344, !alias.scope !50376, !noalias !50379
  %.val26.i.i = load i64, i64* %52, align 8, !dbg !50344, !alias.scope !50379, !noalias !50376
  %53 = icmp eq i64 %.val.i.i, %.val26.i.i, !dbg !50381
  br i1 %53, label %loop_body.i.i, label %clean_ast_897_.loopexit, !dbg !50375

clean_ast_897_.loopexit:                          ; preds = %loop_body.i.i, %next.i.i
  %sret_slot.0.ph = phi i1 [ false, %next.i.i ], [ true, %loop_body.i.i ]
  br label %clean_ast_897_, !dbg !50383

clean_ast_897_:                                   ; preds = %clean_ast_897_.loopexit, %else-block
  %sret_slot.0 = phi i1 [ false, %else-block ], [ %sret_slot.0.ph, %clean_ast_897_.loopexit ]
  ret i1 %sret_slot.0, !dbg !50383

clean_ast_26544_25.loopexit:                      ; preds = %loop_body.loopexit
  br label %clean_ast_26544_25, !dbg !50384

clean_ast_26544_25:                               ; preds = %clean_ast_26544_25.loopexit, %entry-block
  tail call fastcc void @_ZN10sys_common6unwind12begin_unwind12begin_unwind20h7713200070592497824E(i8* noalias nonnull readonly getelementptr inbounds ([12 x i8], [12 x i8]* @str12911, i64 0, i64 0), i64 12, { %str_slice, i32 }* noalias readonly dereferenceable(24) bitcast ({ %str_slice, i32, [4 x i8] }* @"_ZN3map15Tree$LT$u64$GT$8contains10_FILE_LINE20h198f9582d21749b0fqaE" to { %str_slice, i32 }*)), !dbg !50384
  unreachable, !dbg !50384
}
```

The first thing that jumps out at me is that it's way more typed. The Julia AST had all the types it needed, but discards them by the time it reaches LLVM eg:

```
Rust
%26 = getelementptr inbounds %"map::Node<u64>", %"map::Node<u64>"* %14, i64 0, i32 4, !dbg !50240
%27 = load %"map::JuliaArray<Box<map::Node<u64>>>"*, %"map::JuliaArray<Box<map::Node<u64>>>"** %26, align 8, !dbg !50240, !nonnull !361

Julia
%33 = getelementptr inbounds %jl_value_t* %14, i64 2, i32 0
%34 = load %jl_value_t** %33, align 8
```

It also has a lot more metadata about aliasing, nulls, mutability etc.

I don't know how much any of that makes a difference. If it generates better assembly, it's not showing up in the performance counters.

So that's where I'm at for today. Both versions use similar data layouts, generate similar code, use similar amounts of memory and show similar numbers of instructions executed, branches taken/missed and cache references/misses. But one of them is consistently twice as fast as the other.

```
Performance counter stats for '../target/release/imp':

  101,828,660,572      cycles                                                        (66.65%)
   26,539,799,614      instructions              #    0.26  insns per cycle          (83.32%)
    4,320,044,658      branches                                                      (83.33%)
       59,527,376      branch-misses             #    1.38% of all branches          (83.34%)
              326      context-switches                                            
    1,250,325,036      cache-references                                              (83.33%)
      926,747,895      cache-misses              #   74.121 % of all cache refs      (83.34%)

     36.120043773 seconds time elapsed

Performance counter stats for process id '16164':

  150,210,926,888      cycles                                                        (66.66%)
   31,434,058,110      instructions              #    0.21  insns per cycle          (83.33%)
    4,811,978,240      branches                                                      (83.33%)
       57,565,871      branch-misses             #    1.20% of all branches          (83.34%)
              521      context-switches                                            
    1,324,801,988      cache-references                                              (83.34%)
      920,160,263      cache-misses              #   69.456 % of all cache refs      (83.33%)

     59.871103895 seconds time elapsed
```

## Unsafe nodes

Julia has a bunch of limitations that are making this difficult:

* No fixed-length mutable arrays
* Pointerful types are not stored inline
* Limited control over specialization
* No shared layout between types (see the discussion on [struct inheritance in Rust](https://github.com/rust-lang/rfcs/issues/349))

I work around the first two problems by generating custom types that have the layout I want:

``` julia
# equivalent to:
# type Node{N}
#   bitmap::UInt32
#   nodes::NTuple{N, Node} # <- this needs to be stored inline
# end

abstract Node

macro node(n)
  :(begin
  type $(symbol("Node", n)) <: Node
    bitmap::UInt32
    $([symbol("node", i) for i in 1:n]...)
  end
end)
end

# aims to fit into pool sizes - see https://github.com/JuliaLang/julia/blob/6cc48dcd24322976bdc193b3c578acb924f0b8e9/src/gc.c#L1308-L1336
@node(2)
@node(4)
@node(8)
@node(16)
@node(32)
```

I work around the last two problems by hiding the type behind a raw pointer and using my own knowledge of the layout to manually access data:


``` julia
@inline get_node(node::Ptr{Void}, pos::Integer) = begin
  convert(Ptr{Node}, node + nodes_offset + ((pos-1) * sizeof(Ptr)))
end

@inline set_node!(node::Ptr{Void}, val::Ptr{Void}, pos::Integer) = begin
  unsafe_store!(convert(Ptr{Ptr{Void}}, get_node(node, pos)), val)
end
```

I also have to allocate memory and manage write barriers myself, which took a while to get right:

``` julia
@inline is_marked(val_pointer::Ptr{Void}) = begin
  (unsafe_load(convert(Ptr{UInt}, val_pointer), 0) & UInt(1)) == UInt(1)
end

@inline gc_write_barrier(parent_pointer::Ptr{Void}, child_pointer::Ptr{Void}) = begin
  if (is_marked(parent_pointer) && !is_marked(child_pointer))
    ccall((:jl_gc_queue_root, :libjulia), Void, (Ptr{Void},), parent_pointer)
  end
end

grow(parent_pointer::Ptr{Void}, node_pointer::Ptr{Node}, size_before::Integer, size_after::Integer, type_after::Type) = begin
  node_before = unsafe_load(convert(Ptr{Ptr{UInt8}}, node_pointer))
  node_after = ccall((:jl_gc_allocobj, :libjulia), Ptr{UInt8}, (Csize_t,), size_after)
  unsafe_store!(convert(Ptr{Ptr{Void}}, node_after), pointer_from_objref(type_after), 0)
  for i in 1:size_before
    unsafe_store!(node_after, unsafe_load(node_before, i), i)
  end
  for i in (size_before+1):size_after
    unsafe_store!(node_after, 0, i)
  end
  unsafe_store!(convert(Ptr{Ptr{UInt8}}, node_pointer), node_after)
  gc_write_barrier(parent_pointer, convert(Ptr{Void}, node_after))
  return
end
```

I could still inline rows into the node structure by generating custom nodes for each row type (bring on [generated types](https://github.com/JuliaLang/julia/issues/8472) already!) but for now I'm just leaving them individually boxed. I'm not happy with how much gc overhead that creates but it will do for now.

The code is currently messy and gross because I just hacked on it until it worked, but it could be cleaned up if I decide to commit to this route. Writing unsafe code like this in Julia isn't actually that hard (except that I keep forgetting that most types in Julia are boxed). It's much easier to debug memory corruption when you can just poke around in the repl and view raw memory interactively.

I had much more trouble with tracking down allocations in the safe code eg I started by using tuples for rows, but even when they were boxed initially and were going to be boxed in the tree, they were often unboxed in intervening code which causes extra allocations when they have to be reboxed. In the end I just gave up and created custom row types again. (Note to self: Julia tuples are for multiple return, not for data-structures. Stop putting them in data-structures.) I think at some point I would benefit from writing a linting macro that warns me if anootated functions contain allocations or generic dispatch.

There were a few other minor problems. Julia does not have a switch statement and LLVM does not manage to turn this code into a computed goto:

``` julia
# TODO figure out how to get a computed goto out of this
maybe_grow(parent_pointer::Ptr{Void}, node_pointer::Ptr{Node}, length::Integer) = begin
  if length == 2
    grow(parent_pointer, node_pointer, sizeof(Node2), sizeof(Node4), Node4)
  elseif length == 4
    grow(parent_pointer, node_pointer, sizeof(Node4), sizeof(Node8), Node8)
  elseif length == 8
    grow(parent_pointer, node_pointer, sizeof(Node8), sizeof(Node16), Node16)
  elseif length == 16
    grow(parent_pointer, node_pointer, sizeof(Node16), sizeof(Node32), Node32)
  end
  return
end
```

```
julia> code_llvm(Hamt.maybe_grow, (Ptr{Void}, Ptr{Hamt.Node}, Int))

define void @julia_maybe_grow_21919(i8*, %jl_value_t**, i64) {
top:
  %3 = icmp eq i64 %2, 2
  br i1 %3, label %if, label %L

if:                                               ; preds = %top
  %4 = load %jl_value_t** inttoptr (i64 140243957646200 to %jl_value_t**), align 8
  call void @julia_grow_21920(i8* %0, %jl_value_t** %1, i64 24, i64 40, %jl_value_t* %4)
  br label %L8

L:                                                ; preds = %top
  %5 = icmp eq i64 %2, 4
  br i1 %5, label %if1, label %L3

if1:                                              ; preds = %L
  %6 = load %jl_value_t** inttoptr (i64 140243957646248 to %jl_value_t**), align 8
  call void @julia_grow_21920(i8* %0, %jl_value_t** %1, i64 40, i64 72, %jl_value_t* %6)
  br label %L8

L3:                                               ; preds = %L
  %7 = icmp eq i64 %2, 8
  br i1 %7, label %if4, label %L6

if4:                                              ; preds = %L3
  %8 = load %jl_value_t** inttoptr (i64 140243957646296 to %jl_value_t**), align 8
  call void @julia_grow_21920(i8* %0, %jl_value_t** %1, i64 72, i64 136, %jl_value_t* %8)
  br label %L8

L6:                                               ; preds = %L3
  %9 = icmp eq i64 %2, 16
  br i1 %9, label %if7, label %L8

if7:                                              ; preds = %L6
  %10 = load %jl_value_t** inttoptr (i64 140243957646344 to %jl_value_t**), align 8
  call void @julia_grow_21920(i8* %0, %jl_value_t** %1, i64 136, i64 264, %jl_value_t* %10)
  br label %L8

L8:                                               ; preds = %if7, %L6, %if4, %if1, %if
  ret void
}
```

Complex ranges don't seem to inline very well eg this loop compiles to code which still has a function call handling the range:

``` julia
for i in length:-1:pos
  set_node!(node, unsafe_load(convert(Ptr{Ptr{Void}}, get_node(node, i))), i+1)
end
```

```
pass:                                             ; preds = %if
  %30 = add i64 %17, 1
  %31 = load %jl_value_t** %0, align 1
  %32 = bitcast %jl_value_t* %31 to i8*
  %33 = trunc i64 %26 to i32
  %34 = bitcast %jl_value_t* %31 to i32*
  store i32 %33, i32* %34, align 1
  %35 = call i64 @julia_steprange_last3371(i64 %25, i64 -1, i64 %30)
  %36 = icmp slt i64 %25, %35
  %37 = add i64 %35, -1
  %38 = icmp eq i64 %25, %37
  %39 = or i1 %36, %38
  br i1 %39, label %L4, label %L.preheader.L.preheader.split_crit_edge
```

Apart from those two issues, and once I tracked down all the errant allocations, this approach compiles down to pretty tight assembly eg:

``` julia
get_child(node_pointer::Ptr{Node}, ix::Integer) = begin
  node = unsafe_load(convert(Ptr{Ptr{Void}}, node_pointer))
  bitmap = get_bitmap(node)
  if (bitmap & (UInt32(1) << ix)) == 0
    convert(Ptr{Node}, 0)
  else
    get_node(node, get_pos(bitmap, ix))
  end
end
```

```
julia> code_native(Hamt.get_child, (Ptr{Hamt.Node}, Int))
	.text
Filename: /home/jamie/code/imp/src/Hamt.jl
Source line: 98
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$1, %eax
Source line: 98
	movb	%sil, %cl
	shll	%cl, %eax
	xorl	%r8d, %r8d
	cmpq	$31, %rsi
	cmoval	%r8d, %eax
Source line: 96
	movq	(%rdi), %rdx
Source line: 97
	movl	(%rdx), %edi
Source line: 98
	testl	%eax, %edi
	je	L67
	movl	$32, %ecx
Source line: 101
	subq	%rsi, %rcx
	shll	%cl, %edi
	cmpq	$31, %rcx
	cmoval	%r8d, %edi
	popcntl	%edi, %eax
	leaq	8(%rdx,%rax,8), %rax
	popq	%rbp
	ret
L67:	xorl	%eax, %eax
Source line: 99
	popq	%rbp
	ret
```

Performance-wise this has some interesting effects. The best Rust version so far does a full benchmark run in ~65s and peaks at 704MB RSS, or 115s and 1060MB RSS if I force it to use boxed rows. This Julia version takes 85s and peaks at 1279MB RSS, or 65s and 718MB RSS if I delay gc until after building (mutable trees seem to be a pessimal case for generational gcs - they are constantly creating pointers from the old generation into the new generation, requiring costly write barriers). Compare this to 32s for a Julia hashset or 51s for a sorted (and unboxed) Julia array, neither of which can handle prefix searches or persistence.

I'm still a little concerned about the interaction with the gc, but the performance in these crude benchmarks is totally acceptable, especially once I take into account that the Rust version would have to add reference counts once I make the trees semi-persistent and allow sharing data between multiple indexes.

I am frustrated by the amount of work this took. If Julia had fixed-size arrays and would inline pointerful types this could have just been:

``` julia
immutable Node
  bitmap: UInt32
  nodes: FixedArray{Node} # pointer to array of (bitmap, nodes pointer)
end
```

If I end up using Julia heavily it's likely that I will try to improve inline storage. As far as I can tell there is no fundamental reason why this use case isn't supported - it just needs someone to do the work. Pointers are always word-aligned, so one possible implementation would be to change the 'is a pointer' metadata for each field from a boolean to a bitmap of pointer offsets.

This work has taken longer than I expected. There are a few more things I would like to try out, but I'm going to timebox this to the end of the week and then next week just run with whatever has worked best so far.

# Encore 

I quit my job and wandered around looking lost for a while. Now I'm back.

I don't want to get bogged down in performance again, especially when I don't have a real program to benchmark. But on the other hand, benchmarking is fun.

## Sorting and columns

I have a plan and it starts with some sorted arrays.

Ideally I would just throw some tuples or structs into an array and sort it. Unfortunately, Julia still has this restriction on structs that contain pointers. Everything is happy as long as I stick to PODs but as soon as I want to have, say, a string column, I suddenly end up with an array of pointers to heap-allocated rows. Which is not what I want at all. 

``` julia 
r2 = [(id::Int64, id::Int64) for id in ids]
@time sort!(r2, alg=QuickSort)
# 0.056419 seconds (5 allocations: 240 bytes)

r2 = [(id::Int64, string(id)::ASCIIString) for id in ids]
@time sort!(r2, alg=QuickSort)
# 2.340892 seconds (34.94 M allocations: 533.120 MB)
# heap-allocated *and* pass-by-value! tuples are weird!

r2 = [Row(id::Int64, id::Int64) for id in ids]
@time sort!(r2, alg=QuickSort)
# 0.058970 seconds (5 allocations: 240 bytes)

r2 = [Row(id::Int64, string(id)::ASCIIString) for id in ids]
@time sort!(r2, alg=QuickSort)
# 0.124810 seconds (5 allocations: 240 bytes)
```

We can get round this by flipping the layout into columns, but we still need to sort it. Julia's standard sort function only requires length, getindex and setindex:

``` julia 
type Columns2{A,B} <: Columns{Row2{A,B}}
  as::Vector{A}
  bs::Vector{B}
end

function Base.length{A,B}(c2::Columns2{A,B}) 
  length(c2.as)
end

@inline function Base.getindex{A,B}(c2::Columns2{A,B}, ix)
  Row2(c2.as[ix], c2.bs[ix])
end

@inline function Base.setindex!{A,B}(c2::Columns2{A,B}, val::Row2{A,B}, ix)
  c2.as[ix] = val.a
  c2.bs[ix] = val.b
end
```

But these still have to return something row-like which leaves us with exactly the same problem:

``` julia 
c2 = Columns2([id::Int64 for id in ids], [id::Int64 for id in ids])
@time sort!(c2, alg=QuickSort)
# 0.056417 seconds (5 allocations: 240 bytes)

c2 = Columns2([id::Int64 for id in ids], [string(id)::ASCIIString for id in ids])
@time sort!(c2, alg=QuickSort)
# 0.542212 seconds (19.06 M allocations: 582.780 MB, 46.45% gc time)
```

I would enjoy Julia a lot more if this wasn't a thing.

So, let's just brute-force a workaround. I'll copy the sorting code from the base library and generate different versions of it for every number of columns, using multiple variables to hold the values instead of tuples or structs.

``` julia 
function define_columns(n)
  cs = [symbol("c", c) for c in 1:n]
  ts = [symbol("C", c) for c in 1:n]
  tmps = [symbol("tmp", c) for c in 1:n]
  
  :(begin
  
  @inline function lt($(cs...), i, j) 
    @inbounds begin 
      $([:(if !isequal($(cs[c])[i], $(cs[c])[j]); return isless($(cs[c])[i], $(cs[c])[j]); end) for c in 1:(n-1)]...)
      return isless($(cs[n])[i], $(cs[n])[j])
    end
  end
  
  @inline function lt2($(cs...), $(tmps...), j) 
    @inbounds begin 
      $([:(if !isequal($(tmps[c]), $(cs[c])[j]); return isless($(tmps[c]), $(cs[c])[j]); end) for c in 1:(n-1)]...)
      return isless($(tmps[n]), $(cs[n])[j])
    end
  end
  
  @inline function swap2($(cs...), i, j)
    @inbounds begin
      $([:(begin
      $(tmps[c]) = $(cs[c])[j]
      $(cs[c])[j] = $(cs[c])[i]
      $(cs[c])[i] = $(tmps[c])
    end) for c in 1:n]...)
  end
  end

  @inline function swap3($(cs...), i, j, k)
    @inbounds begin
      $([:(begin
      $(tmps[c]) = $(cs[c])[k]
      $(cs[c])[k] = $(cs[c])[j]
      $(cs[c])[j] = $(cs[c])[i]
      $(cs[c])[i] = $(tmps[c])
    end) for c in 1:n]...)
  end
  end
  
  # sorting cribbed from Base.Sort
  
  function insertion_sort!($(cs...), lo::Int, hi::Int)
      @inbounds for i = lo+1:hi
        j = i
        $([:($(tmps[c]) = $(cs[c])[i]) for c in 1:n]...)
        while j > lo
            if lt2($(cs...), $(tmps...), j-1)
              $([:($(cs[c])[j] = $(cs[c])[j-1]) for c in 1:n]...)
              j -= 1
              continue
            end
            break
        end
        $([:($(cs[c])[j] = $(tmps[c])) for c in 1:n]...)
    end
  end

  @inline function select_pivot!($(cs...), lo::Int, hi::Int)
      @inbounds begin
          mi = (lo+hi)>>>1
          if lt($(cs...), mi, lo)
              swap2($(cs...), lo, mi)
          end
          if lt($(cs...), hi, mi)
              if lt($(cs...), hi, lo)
                  swap3($(cs...), lo, mi, hi)
              else
                  swap2($(cs...), mi, hi)
              end
          end
          swap2($(cs...), lo, mi)
      end
      return lo
  end

  function partition!($(cs...), lo::Int, hi::Int)
      pivot = select_pivot!($(cs...), lo, hi)
      i, j = lo, hi
      @inbounds while true
          i += 1; j -= 1
          while lt($(cs...), i, pivot); i += 1; end;
          while lt($(cs...), pivot, j); j -= 1; end;
          i >= j && break
          swap2($(cs...), i, j)
      end
      swap2($(cs...), pivot, j)
      return j
  end

  function quicksort!($(cs...), lo::Int, hi::Int)
      @inbounds while lo < hi
          if hi-lo <= 20 
            insertion_sort!($(cs...), lo, hi)
            return 
          end
          j = partition!($(cs...), lo, hi)
          if j-lo < hi-j
              lo < (j-1) && quicksort!($(cs...), lo, j-1)
              lo = j+1
          else
              j+1 < hi && quicksort!($(cs...), j+1, hi)
              hi = j-1
          end
      end
      return
  end
  
  function quicksort!{$(ts...)}(cs::Tuple{$(ts...)})
    quicksort!($([:(cs[$c]) for c in 1:n]...), 1, length(cs[1]))
    return cs
  end
  end)
end

for i in 1:10
  eval(define_columns(i))
end
```

It's not pretty. But...

``` julia 
c2 = ([id::Int64 for id in ids], [id::Int64 for id in ids])
@time quicksort!(c2)
# 0.017385 seconds (4 allocations: 160 bytes)

c2 = ([id::Int64 for id in ids], [string(id)::ASCIIString for id in ids])
@time quicksort!(c2)
# 0.053001 seconds (4 allocations: 160 bytes)
```

Onwards.

## Intersection 

I kinda thought that Julia specialized on closures, but this turns out not to be true in the current release. So I upgraded to v0.5-rc0 and then spent most of the day persuading Juno to cooperate. I lost a lot of time before realizing that the Ubuntu 'nightly' PPA hasn't been updated in two months. After switching to the generic linux build and patching Juno in a few places it mostly works now, apart from a weird issue where displaying results inline in Atom sometimes leaves Julia spinning for minutes. 

But with that out of the way, we can write a really cute version of leapfrog triejoin:

``` julia 
# gallop cribbed from http://www.frankmcsherry.org/dataflow/relational/join/2015/04/11/genericjoin.html
function gallop{T}(column::Vector{T}, value::T, lo::Int64, hi::Int64, cmp) 
  if (lo < hi) && cmp(column[lo], value)
    step = 1
    while (lo + step < hi) && cmp(column[lo + step], value)
      lo = lo + step 
      step = step << 1
    end
    
    step = step >> 1
    while step > 0
      if (lo + step < hi) && cmp(column[lo + step], value)
        lo = lo + step 
      end
      step = step >> 1
    end
    
    lo += 1
  end
  lo 
end 

@inline function intersect{T,N}(handler, cols::NTuple{N, Vector{T}}, los::Vector{Int64}, his::Vector{Int64})
  # assume los/his are valid 
  # los inclusive, his exclusive
  n = length(cols)
  local value::T
  value = cols[n][los[n]]
  inited = false
  while true 
    for c in 1:n 
      if inited && (cols[c][los[c]] == value)
        matching_his = [gallop(cols[c], value, los[c], his[c], <=) for c in 1:n]
        handler(value, los, matching_his)
        los[c] = matching_his[c]
        # TODO can we set los = matching_his without breaking the stop condition?
      else 
        los[c] = gallop(cols[c], value, los[c], his[c], <)
      end
      if los[c] >= his[c]
        return 
      else 
        value = cols[c][los[c]]
      end
    end
    inited = true
  end
end
```

It's really unoptimised at the moment - I need to reuse allocations, remove bounds/null checks, unroll loops etc. But it seems to work:

``` julia 
function f() 
  edges_x = [[1, 2, 3, 3, 4], [2, 3, 1, 4, 2]]
  edges_y = [[1, 2, 3, 3, 4], [2, 3, 1, 4, 2]]
  edges_z = [[1, 2, 2, 3, 4], [3, 1, 4, 2, 3]]
  intersect((edges_x[1], edges_z[1]), [1,1], [6,6]) do x, x_los, x_his
    intersect((edges_x[2], edges_y[1]), [x_los[1],1], [x_his[1],6]) do y, y_los, y_his
      intersect((edges_y[2], edges_z[2]), [y_los[2], x_los[2]], [y_his[2], x_his[2]]) do z, z_los, z_his
        println(x,y,z)
      end
    end
  end
end
```

It needed a bit of help typing `value` for some reason, and it insists on boxing it, but the code for `f` looks good otherwise. No generic calls and all the intersections are inlined.

## Closures and sadness

Ooops, the anonymous functions aren't inlined. Can fix that pretty easily:

``` julia 
@time intersect((edges_x[1], edges_z[1]), (1,1), (n,n), @inline function (x, x_los, x_his)
  intersect((edges_x[2], edges_y[1]), (x_los[1],1), (x_his[1],n), @inline function (y, y_los, y_his)
    intersect((edges_y[2], edges_z[2]), (y_los[2], x_los[2]), (y_his[2], x_his[2]), @inline function (z, z_los, z_his)
      println(x,y,z)
    end)
  end)
end)
```

I had to change the syntax because `@inline` is fussy about about what it accepts. I guess it wasn't intended for use with anonymous functions, because they were specialized on there was no opportunity to inline them anyway.

I cleaned up most of the obvious allocations by changing arrays to tuples, and unpacking them in the function body. That requires unrolling the inner loops too, which is probably not harmful. 

``` julia 
@generated function intersect{T,N}(cols::NTuple{N, Vector{T}}, los::NTuple{N, Int64}, his::NTuple{N, Int64}, handler)
  # assume los/his are valid 
  # los inclusive, his exclusive
  quote
    $(Expr(:meta, :inline))
    @inbounds begin 
      local value::$T
      @nextract $N col cols
      @nextract $N lo los 
      @nextract $N hi his 
      value = col_1[lo_1]
      inited = false
      while true 
        @nexprs $N c->
        begin
          if inited && (col_c[lo_c] == value)
            @nexprs $N c2-> matching_hi_c2 = gallop(col_c2, value, lo_c2, hi_c2, <=)
            handler(value, (@ntuple $N lo), (@ntuple $N matching_hi))
            lo_c = matching_hi_c
            # TODO can we set los = matching_his without breaking the stop condition?
          else 
            lo_c = gallop(col_c, value, lo_c, hi_c, <)
          end
          if lo_c >= hi_c
            return 
          else 
            value = col_c[lo_c]
          end
          inited = true
        end
      end
    end
  end
end
```

It's nice that the facilities exist to do this kind of code rewriting, but I wouldn't have to do it in the first place if I could just mutate some stack-allocated tuple-like thing. Like a grownup.

Annoyingly, there is still a lot of allocation going on. Looking at the generated code it seems that, while all the anonymous functions have been inlined, the closures are still being created. And heap-allocated :(
  
It also looks like any values that are closed over become boxed, presumably because Julia can't guarantee that the closure doesn't escape the lifetime of the current stackframe. But the box doesn't get a type and that messed up downsteam inference - note the return type of `f` is `ANY` rather than `Int64`.

``` julia 
function f(xs)
  const t = 0
  foreach(xs) do x
    t += x
  end
  t
end
```

``` julia
Variables:
  #self#::Relation.#f
  xs::Array{Int64,1}
  t::CORE.BOX
  #43::Relation.##43#44

Body:
  begin 
      t::CORE.BOX = $(Expr(:new, :(Core.Box)))
      (Core.setfield!)(t::CORE.BOX,:contents,0)::Int64 # line 213:
      #43::Relation.##43#44 = $(Expr(:new, :(Relation.##43#44), :(t)))
      SSAValue(0) = #43::Relation.##43#44
      $(Expr(:invoke, LambdaInfo for foreach(::Relation.##43#44, ::Array{Int64,1}), :(Relation.foreach), SSAValue(0), :(xs))) # line 216:
      return (Core.getfield)(t::CORE.BOX,:contents)::ANY
  end::ANY
```

It looks like Julia's closures just aren't there yet.

## Working around closures

I managed a macro-y version that does the trick, producing zero allocations in the main body. The nice `@nexprs` macro I was using before doesn't interact well with the macro hygienisation so I have to do stuff by hand, with much additional syntax.

``` julia 
function unpack(expr)
  assert(expr.head == :tuple)
  for value in expr.args
    assert(typeof(value) == Symbol)
  end
  expr.args
end

macro intersect(cols, los, ats, his, next_los, next_his, handler)
  cols = unpack(cols)
  los = unpack(los)
  ats = unpack(ats)
  his = unpack(his)
  next_los = unpack(next_los)
  next_his = unpack(next_his)
  n = length(cols)
  quote
    # assume los/his are valid 
    # los inclusive, his exclusive
    @inbounds begin 
      $([
      quote
        $(esc(ats[c])) = $(esc(los[c]))
      end
      for c in 1:n]...)
      value = $(esc(cols[n]))[$(esc(ats[n]))]
      fixed = 1
      finished = false
      while !finished
        $([
        quote
          if fixed == $n 
            $([
            quote
              $(esc(next_los[c2])) = $(esc(ats[c2]))
              $(esc(next_his[c2])) = gallop($(esc(cols[c2])), value, $(esc(ats[c2])), $(esc(his[c2])), <=)
              $(esc(ats[c2])) = $(esc(next_his[c2]))
            end
            for c2 in 1:n]...)
            $handler # TODO huge code duplication
          else 
            $(esc(ats[c])) = gallop($(esc(cols[c])), value, $(esc(ats[c])), $(esc(his[c])), <)
          end
          if $(esc(ats[c])) >= $(esc(his[c]))
            finished = true
          else 
            next_value = $(esc(cols[c]))[$(esc(ats[c]))]
            fixed = (value == next_value) ? fixed+1 : 1
            value = next_value
          end
          inited = true
        end
        for c in 1:n]...)
      end
    end
  end
end
```

This is fast but awful to look at, so I played around with closures some more. I discovered that boxing of closed-over variables only happens if a stack-allocated thing is mutated. Heap-allocated things propagate their types just fine. (I'm sure I had a case where a stack-allocated thing got boxed without being mutated. Not sure if I imagined it or if the nest of closures was confusing the mutation analysis.)

``` julia 
function f(xs)
  t = [0]
  foreach(xs) do x
    t[1] += x
  end
  t[1]
end
```

``` julia 
Variables:
  #self#::Relation.#f
  xs::Array{Int64,1}
  t::Array{Int64,1}
  #267::Relation.##267#268{Array{Int64,1}}

Body:
  begin 
      t::Array{Int64,1} = $(Expr(:invoke, LambdaInfo for vect(::Int64, ::Vararg{Int64,N}), :(Base.vect), 0)) # line 215:
      #267::Relation.##267#268{Array{Int64,1}} = $(Expr(:new, Relation.##267#268{Array{Int64,1}}, :(t)))
      SSAValue(0) = #267::Relation.##267#268{Array{Int64,1}}
      $(Expr(:invoke, LambdaInfo for foreach(::Relation.##267#268{Array{Int64,1}}, ::Array{Int64,1}), :(Relation.foreach), SSAValue(0), :(xs))) # line 218:
      return (Base.arrayref)(t::Array{Int64,1},1)::Int64
  end::Int64
```

This is reflected in the emitted code - the non-boxed version has a constant 6 allocations whereas the boxed version allocates for each x in xs. 

To avoid having to create closures on each nexted iteration, I moved all the state variables to heap-allocated arrays at the top of the query.

``` julia
function f(edges_xy::Tuple{Vector{Int64}, Vector{Int64}}, edges_yz::Tuple{Vector{Int64}, Vector{Int64}}, edges_xz::Tuple{Vector{Int64}, Vector{Int64}}) 
  cols = (edges_xy[1], edges_xy[2], Int64[], edges_yz[1], edges_yz[2], Int64[], edges_xz[1], edges_xz[2], Int64[])
  los = [1 for _ in 1:length(cols)]
  ats = [1 for _ in 1:length(cols)]
  his = [length(cols[i])+1 for i in 1:length(cols)]
  count = [0]

  @time begin
    intersect(cols, los, ats, his, (1, 7)) do
      intersect(cols, los, ats, his, (2, 4)) do 
        intersect(cols, los, ats, his, (5, 8)) do
          count[1] += 1
        end 
      end 
    end
  end
  
  count[1]
end
```

Those nested closures still get created every time though (even though they are all identical) causing many many heap allocations. Rewriting like this fixed the problem:

``` julia 
function f(edges_xy::Tuple{Vector{Int64}, Vector{Int64}}, edges_yz::Tuple{Vector{Int64}, Vector{Int64}}, edges_xz::Tuple{Vector{Int64}, Vector{Int64}}) 
  cols = (edges_xy[1], edges_xy[2], Int64[], edges_yz[1], edges_yz[2], Int64[], edges_xz[1], edges_xz[2], Int64[])
  los = [1 for _ in 1:length(cols)]
  ats = [1 for _ in 1:length(cols)]
  his = [length(cols[i])+1 for i in 1:length(cols)]
  count = [0]
  
  cont4 = () -> count[1] += 1
  cont3 = () -> intersect(cont4, cols, los, ats, his, (5, 8))
  cont2 = () -> intersect(cont3, cols, los, ats, his, (2, 4))
  cont1 = () -> intersect(cont2, cols, los, ats, his, (1, 7))
  
  @time cont1()
  
  count[1]
end
```

Now `intersect` gets to be a normal function again.

``` julia 
function intersect(next, cols, los, ats, his, ixes)
  # assume los/his are valid 
  # los inclusive, his exclusive
  @inbounds begin
    for ix in ixes
      ats[ix] = los[ix]
    end
    n = length(ixes)
    value = cols[ixes[n]][ats[ixes[n]]]
    fixed = 1
    while true 
      for ix in ixes
        if fixed == n
          for ix2 in ixes
            los[ix2+1] = ats[ix2]
            his[ix2+1] = gallop(cols[ix2], value, ats[ix2], his[ix2], <=)
            ats[ix2] = his[ix2+1]
          end
          next()
        else 
          ats[ix] = gallop(cols[ix], value, ats[ix], his[ix], <)
        end
        if ats[ix] >= his[ix]
          return 
        else 
          next_value = cols[ix][ats[ix]]
          fixed = (value == next_value) ? fixed+1 : 1
          value = next_value
        end
      end
    end
  end
end
```

This is only slightly slower than the macro version.

Belatedly, I realise that now that the state is kept outside the function I could just have avoided the closures all together:

``` julia 
function start_intersect(cols, los, ats, his, ixes)
  # assume los/his are valid 
  # los inclusive, his exclusive
  @inbounds begin
    for ix in ixes
      ats[ix] = los[ix]
    end
  end
end

function next_intersect(cols, los, ats, his, ixes)
  @inbounds begin
    fixed = 1
    n = length(ixes)
    value = cols[n][ats[ixes[n]]]
    while true 
      for c in 1:n
        ix = ixes[c]
        if fixed == n
          for c2 in 1:n
            ix2 = ixes[c2]
            los[ix2+1] = ats[ix2]
            his[ix2+1] = gallop(cols[c2], value, ats[ix2], his[ix2], <=)
            ats[ix2] = his[ix2+1]
          end
          return true
        else 
          ats[ix] = gallop(cols[c], value, ats[ix], his[ix], <)
        end
        if ats[ix] >= his[ix]
          return false
        else 
          next_value = cols[c][ats[ix]]
          fixed = (value == next_value) ? fixed+1 : 1
          value = next_value
        end
      end
    end
  end
end
```

Wish I had thought of that two days ago.

The setup is now kind of ugly, but the query compiler is going to be handling this anyway.

``` julia 
function f(edges_xy::Tuple{Vector{Int64}, Vector{Int64}}, edges_yz::Tuple{Vector{Int64}, Vector{Int64}}, edges_xz::Tuple{Vector{Int64}, Vector{Int64}}) 
  cols_x = [edges_xy[1], edges_xz[1]]
  cols_y = [edges_xy[2], edges_yz[1]]
  cols_z = [edges_yz[2], edges_xz[2]]
  ixes_x = [1,7]
  ixes_y = [2,4]
  ixes_z = [5,8]
  los = [1 for _ in 1:9]
  ats = [1 for _ in 1:9]
  his = [length(cols_x[1])+1 for i in 1:9]
  count = 0
  
  @time begin
    start_intersect(cols_x, los, ats, his, ixes_x)
    while next_intersect(cols_x, los, ats, his, ixes_x)
      x = cols_x[1][los[2]]
      start_intersect(cols_y, los, ats, his, ixes_y)
      while next_intersect(cols_y, los, ats, his, ixes_y)
        y = cols_y[1][los[3]]
        start_intersect(cols_z, los, ats, his, ixes_z)
        while next_intersect(cols_z, los, ats, his, ixes_z)
          z = cols_z[1][los[6]]
          # println((x,y,z))
          count += 1
        end
      end
    end
  end
  
  count
end
```

## 2016 Jul 31

Section titles are a global namespace, so I'm switching to dates :)

I had some time this evening so I hashed out the core codegen.

To write code I have to figure out what data to compute, how to compute it, how to store it, in what order to compute it, how to organise the code, what to name things etc. I find that if I just sit down with an editor and try to do this all at once I spend a lot of time context switching, which manifests as these mental stack overflows where I just go blank for a while. 

Over the last year or so, I gradually started to batch these tasks together. I start by choosing a couple of examples and writing down the inputs and outputs. Then I sketch out what data will help me to get from input to output.

``` julia
q = quote 
  edge(a,b)
  edge(b,c)
  edge(c,a)
end

fieldnames(q)
q.head
q.args
q.args[2].head
q.args[2].args

a => (r1, c1), (r3, c2)
b => ...
c => ...

var order 

indexes
r1 => (1,2)
...

ixes 
r1, c1 => 1
r1, c2 => 2
r1, end => 3
...
r3, c2 => 7
r3, c1 => 8
r3, end => 9

cols = [r1, ...]
quicksort!((cols[1][2], cols[1][1]))
...
cols_a = (cols[1][2], cols[1][1])
ixes_a = (1, 7)
...
los, ats = 1
his = length(cols[r][c])
results = []

start_intersect 
while next_intersect 
  a = cols[1][2][los[3]]
  ...
     push!(results, (a,b,c))
end
```

Then I pick names and topo-sort the chunks of data. That whole plan then goes on one side of the screen and I can start cranking out code on the other side of the screen. The plan fills in my patchy short-term memory so the code flows smoothly. I didn't quite manage to type the compiler without hitting backspace, but it was close.

``` julia
function plan(query, variables)
  relations = [line.args[1] for line in query.args if line.head != :line]
  
  sources = Dict()
  for (clause, line) in enumerate(query.args) 
    if line.head != :line
      assert(line.head == :call)
      for (column, variable) in enumerate(line.args[2:end])
        assert(variable in variables)
        push!(get!(()->[], sources, variable), (clause,column))
      end
    end
  end
  
  sort_orders = Dict()
  for variable in variables 
    for (clause, column) in sources[variable]
      push!(get!(()->[], sort_orders, clause), column)
    end
  end
  
  ixes = Dict()
  next_ix = 1
  for (clause, columns) in sort_orders
    for column in columns
      ixes[(clause, column)] = next_ix
      next_ix += 1
    end
    ixes[(clause, :buffer)] = next_ix
    next_ix += 1
  end
  
  column_inits = Vector(length(ixes))
  for ((clause, column), ix) in ixes
    if column == :buffer
      column_inits[ix] = :()
    else
      clause_name = query.args[clause].args[1]
      column_inits[ix] = :(copy($(esc(clause_name))[$column]))
    end
  end
  
  sorts = []
  for (clause, columns) in sort_orders
    sort_ixes = [ixes[(clause, column)] for column in columns]
    sort_args = [:(columns[$ix]) for ix in sort_ixes]
    sort = :(quicksort!(tuple($(sort_args...))))
    push!(sorts, sort)
  end
  
  variable_inits = []
  for variable in variables 
    clauses_and_columns = sources[variable]
    variable_ixes = [ixes[(clause, column)] for (clause, column) in clauses_and_columns]
    variable_columns = [:(columns[$ix]) for ix in variable_ixes]
    variable_init = quote
      $(symbol("columns_", variable)) = [$(variable_columns...)]
      $(symbol("ixes_", variable)) = [$(variable_ixes...)]
    end
    push!(variable_inits, variable_init)
  end
  
  setup = quote
    columns = tuple($(column_inits...))
    $(sorts...)
    los = Int64[1 for i in 1:$(length(ixes))]
    ats = Int64[1 for i in 1:$(length(ixes))]
    his = Int64[length(columns[i]) for i in 1:$(length(ixes))]
    $(variable_inits...)
    results = []
  end
  
  function body(variable_ix)
    if variable_ix <= length(variables)
      variable = variables[variable_ix]
      variable_columns = symbol("columns_", variable)
      variable_ixes = symbol("ixes_", variable)
      result_column = ixes[sources[variable][1]]
      quote
        start_intersect($variable_columns, los, ats, his, $variable_ixes)
        while next_intersect($variable_columns, los, ats, his, $variable_ixes)
          $(esc(variable)) = columns[$result_column][los[$(result_column+1)]]
          $(body(variable_ix + 1))
        end
      end
    else 
      quote
        push!(results, tuple($([esc(variable) for variable in variables]...)))
      end 
    end
  end
          
  quote 
    $setup
    @time $(body(1))
    results
  end
end
```

With a crappy little macro we can now write the previous query as:

``` julia 
macro query(variables, query)
  plan(query, variables.args)
end

function f(edge) 
  @query([a,b,c], 
  begin
    edge(a,b)
    edge(b,c)
    edge(c,a)
  end)
end
```

Thats the basics. The next big steps are embedding an expression language and choosing the variable ordering automatically.  

EDIT: I found a little more time, so here is the chinook query from earlier in the year:

``` julia
album = read_columns("data/Album.csv", [Int64, String, Int64])
artist = read_columns("data/Artist.csv", [Int64, String])
track = read_columns("data/Track.csv", [Int64, String, Int64])
playlist_track = read_columns("data/PlaylistTrack.csv", [Int64, Int64])
playlist = read_columns("data/Playlist.csv", [Int64, String])

metal = read_columns("data/Metal.csv", [String])

function who_is_metal(album, artist, track, playlist_track, playlist, metal)
  @query([pn, p, t, al, a, an],
  begin
    metal(pn)
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al)
    album(al, _, a)
    artist(a, an)
  end
  )[6]
end
```

Runs in 0.37ms, of which only about 0.01ms is solving the query and the rest is copying and sorting the inputs. My notes say the rust version took 0.8ms all together and SQLite took 1.2ms just to solve the query (both on an older machine). I won't bother benchmarking properly until the compiler is feature complete and I have tests, but looks like I'm inside my performance budget so far.

## 2016 Aug 1

Expressions just act as extra filters on results, so I can write things like:

``` julia
edge(a,b)
a < b
edge(b,c)
b < c
edge(c,a)
```

Evaluating them is pretty straightforward. I grab all the variables in the expression, assume any that aren't in the variable order are external constants, and figure out the earliest time when the remainder have been assigned.

``` julia
filters = [[] for _ in variables]
for clause in expression_clauses
  line = query.args[clause]
  callable_at = maximum(indexin(collect_variables(line), variables))
  push!(filters[callable_at], line)
end

function filter(filters, tail)
  if length(filters) == 0
    tail
  else 
    quote 
      if $(filters[1])
        $(filter(filters[2:end], tail))
      end
    end
  end
end

function body(variable_ix)
  ...
          $(filter(filters[variable_ix], body(variable_ix + 1)))
  ...
end
```

Equations are a bit trickier. An expression like `a == b + 1` could be treated as a filter on the results, but in many cases it would be much better to run it as soon as `b` is assigned, before wasting time generating many `a`s. On the other hand, that limits the compiler to variable orders where `b` comes before `a`, which may be inefficient. 

One of my core goals is to make performance predictable, so rather than deciding this in the compiler with some heuristic I'm going to have the programmer communicate intent directly. `a == b + 1` is a filter that will be run once `a` and `b` are both defined. `a = b + 1` is an assignment that forces `b` to be assigned before `a` and that will be run just before the intersection for `a`. In a true relational language this distinction wouldn't exist, but I want to be pragmatic for now.

``` julia
function assign(cols, los, ats, his, ixes, value)
  @inbounds begin
    n = length(ixes)
    for c in 1:n
      ix = ixes[c]
      los[ix+1] = gallop(cols[c], value, los[ix], his[ix], <)
      if los[ix+1] >= his[ix]
        return false
      end
      his[ix+1] = gallop(cols[c], value, los[ix+1], his[ix], <=)
    end
    return true
  end
end

function body(variable_ix)
  ...
    if haskey(assignment_clauses, variable)
      quote
        let $variable = $(assignment_clauses[variable])
          if assign($variable_columns, los, ats, his, $variable_ixes, $variable)
            # println($(repeat("  ", variable_ix)), $(string(variable)), "=", $variable)
            $tail
          end
        end
      end
    else
      ...
    end
  ...
end
```

Now we can do:

``` julia 
begin
  pn = "Heavy Metal Classic"
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end
```

What next? I have some ideas about variable ordering, but I need a lot more examples to see if they are realistic. Maybe projection/aggreation? I need to think a bit about how I want to handle that.

## 2016 Aug 4

Projection is really easy - we can just reuse the same building blocks:

``` julia 
metal = @query([pn, p, t, al, a, an],
begin
  pn = "Heavy Metal Classic"
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end
)

metal_projected = @query([an], 
begin
  metal(_, _, _, _, _, an)
end)
```

While I was doing that, I noticed that I'm returning columns of type `Any`. Fixing that is pretty tricky, because I don't actually know the type of the variables when I generate the query code. I'm relying on Julia's type inference, but type inference only happens after I generate code. I could wait until the first result to initialize the columns, but that doesn't work for queries with no results. 

Let's just work around it for now by allowing the user to specify the types in the query:

``` julia 
function plan(query, typed_variables)
  variables = []
  variable_types = []
  for typed_variable in typed_variables
    if isa(typed_variable, Symbol)
      push!(variables, typed_variable)
      push!(variable_types, :Any)
    elseif isa(typed_variable, Expr) && typed_variable.head == :(::)
      push!(variables, typed_variable.args[1])
      push!(variable_types, typed_variable.args[2])
    else 
      throw("Variable must be a symbol (with optional type annotation)")
    end
  end 
  ... 
     $(symbol("results_", variable)) = Vector{$(variable_type)}()
  ...
end
```

``` julia
@join([a,b,c], [a::Int64,b::Int64,c::Int64], 
begin
  edge(a,b)
  a < b
  edge(b,c)
  b < c
  edge(c,a)
end)
```

We can also do Yannakis-style queries:

``` julia 
function who_is_metal2(album, artist, track, playlist_track, playlist)
  i1 = @query([pn::String, p::Int64],
  begin
    pn = "Heavy Metal Classic"
    playlist(p, pn)
  end)
  i2 = @query([p::Int64, t::Int64],
  begin 
    i1(_, p)
    playlist_track(p, t)
  end)
  i3 = @query([t::Int64, al::Int64],
  begin 
    i2(_, t)
    track(t, _, al)
  end)
  i4 = @query([al::Int64, a::Int64],
  begin 
    i3(_, al)
    album(al, _, a)
  end)
  i5 = @query([a::Int64, an::String],
  begin 
    i4(_, a)
    artist(a, an)
  end)
  @query([an::String],
  begin
    i5(_, an)
  end)
end
```

On to aggregation. I have a planner that turns this:

``` julia
@join([pn],
[p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
(0.0,+,price::Float64),
begin
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al, _, _, _, _, _, price)
end)
```

Into this:

``` 
begin  # /home/jamie/imp/src/Imp.jl, line 586:
    begin  # /home/jamie/imp/src/Imp.jl, line 411:
        begin  # /home/jamie/imp/src/Imp.jl, line 333:
            #11256#columns = tuple(copy(playlist_track[1]),copy(playlist_track[2]),(),copy(playlist[1]),copy(playlist[2]),(),copy(track[1]),copy(track[3]),copy(track[9]),()) # /home/jamie/imp/src/Imp.jl, line 334:
            quicksort!(tuple(#11256#columns[1],#11256#columns[2]))
            quicksort!(tuple(#11256#columns[4],#11256#columns[5]))
            quicksort!(tuple(#11256#columns[7],#11256#columns[8],#11256#columns[9])) # /home/jamie/imp/src/Imp.jl, line 335:
            #11257#los = Int64[1 for #11258#i = 1:10] # /home/jamie/imp/src/Imp.jl, line 336:
            #11259#ats = Int64[1 for #11258#i = 1:10] # /home/jamie/imp/src/Imp.jl, line 337:
            #11260#his = Int64[length(#11256#columns[#11258#i]) + 1 for #11258#i = 1:10] # /home/jamie/imp/src/Imp.jl, line 338:
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11261#columns_p = [#11256#columns[4],#11256#columns[1]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11262#ixes_p = [4,1] # /home/jamie/imp/src/Imp.jl, line 325:
                nothing
            end
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11263#columns_pn = [#11256#columns[5]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11264#ixes_pn = [5] # /home/jamie/imp/src/Imp.jl, line 325:
                #11265#results_pn = Vector{String}()
            end
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11266#columns_t = [#11256#columns[2],#11256#columns[7]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11267#ixes_t = [2,7] # /home/jamie/imp/src/Imp.jl, line 325:
                nothing
            end
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11268#columns_al = [#11256#columns[8]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11269#ixes_al = [8] # /home/jamie/imp/src/Imp.jl, line 325:
                nothing
            end
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11270#columns_price = [#11256#columns[9]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11271#ixes_price = [9] # /home/jamie/imp/src/Imp.jl, line 325:
                nothing
            end # /home/jamie/imp/src/Imp.jl, line 339:
            #11272#results_aggregate = Vector{Float64}()
        end # /home/jamie/imp/src/Imp.jl, line 412:
        begin  # /home/jamie/imp/src/Imp.jl, line 392:
            start_intersect(#11261#columns_p,#11257#los,#11259#ats,#11260#his,#11262#ixes_p) # /home/jamie/imp/src/Imp.jl, line 393:
            while next_intersect(#11261#columns_p,#11257#los,#11259#ats,#11260#his,#11262#ixes_p) # /home/jamie/imp/src/Imp.jl, line 394:
                #11273#p = (#11256#columns[4])[#11257#los[5]] # /home/jamie/imp/src/Imp.jl, line 395:
                begin  # /home/jamie/imp/src/Imp.jl, line 392:
                    start_intersect(#11263#columns_pn,#11257#los,#11259#ats,#11260#his,#11264#ixes_pn) # /home/jamie/imp/src/Imp.jl, line 393:
                    while next_intersect(#11263#columns_pn,#11257#los,#11259#ats,#11260#his,#11264#ixes_pn) # /home/jamie/imp/src/Imp.jl, line 394:
                        #11274#pn = (#11256#columns[5])[#11257#los[6]] # /home/jamie/imp/src/Imp.jl, line 395:
                        begin  # /home/jamie/imp/src/Imp.jl, line 364:
                            #11275#aggregate = 0.0 # /home/jamie/imp/src/Imp.jl, line 365:
                            begin  # /home/jamie/imp/src/Imp.jl, line 392:
                                start_intersect(#11266#columns_t,#11257#los,#11259#ats,#11260#his,#11267#ixes_t) # /home/jamie/imp/src/Imp.jl, line 393:
                                while next_intersect(#11266#columns_t,#11257#los,#11259#ats,#11260#his,#11267#ixes_t) # /home/jamie/imp/src/Imp.jl, line 394:
                                    #11276#t = (#11256#columns[2])[#11257#los[3]] # /home/jamie/imp/src/Imp.jl, line 395:
                                    begin  # /home/jamie/imp/src/Imp.jl, line 392:
                                        start_intersect(#11268#columns_al,#11257#los,#11259#ats,#11260#his,#11269#ixes_al) # /home/jamie/imp/src/Imp.jl, line 393:
                                        while next_intersect(#11268#columns_al,#11257#los,#11259#ats,#11260#his,#11269#ixes_al) # /home/jamie/imp/src/Imp.jl, line 394:
                                            #11277#al = (#11256#columns[8])[#11257#los[9]] # /home/jamie/imp/src/Imp.jl, line 395:
                                            begin  # /home/jamie/imp/src/Imp.jl, line 392:
                                                start_intersect(#11270#columns_price,#11257#los,#11259#ats,#11260#his,#11271#ixes_price) # /home/jamie/imp/src/Imp.jl, line 393:
                                                while next_intersect(#11270#columns_price,#11257#los,#11259#ats,#11260#his,#11271#ixes_price) # /home/jamie/imp/src/Imp.jl, line 394:
                                                    #11278#price = (#11256#columns[9])[#11257#los[10]] # /home/jamie/imp/src/Imp.jl, line 395:
                                                    #11275#aggregate = #11275#aggregate + #11278#price::Float64
                                                end
                                            end
                                        end
                                    end
                                end
                            end # /home/jamie/imp/src/Imp.jl, line 366:
                            if #11275#aggregate != 0.0 # /home/jamie/imp/src/Imp.jl, line 367:
                                push!(#11265#results_pn,#11274#pn) # /home/jamie/imp/src/Imp.jl, line 370:
                                push!(#11272#results_aggregate,#11275#aggregate)
                            end
                        end
                    end
                end
            end
        end # /home/jamie/imp/src/Imp.jl, line 413:
        tuple(#11265#results_pn,#11272#results_aggregate)
    end
end
```

It only aggregates after the last variable in the ordering that is returned, so if I want to aggregate over variables that are earlier in the ordering I need to apply another join to the result.

``` julia 
result = @join([p, pn, t],
[p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
(0.0,+,price::Float64),
begin
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al, _, _, _, _, _, price)
end)

@join([t],
[t::Int64, p::Int64, pn::String, price::Float64],
(0.0,+,price::Float64),
begin
  result(p, pn, t, price)
end)
```

I want to wrap that up in another ugly macro but right now I'm just flailing and nothing works. Tomorrow...

## 2016 Aug 5

Ok, I finally got this nailed down. There were a bunch of little things I had to fix.

The inputs to queries are sets, but the query effectively projects out the columns it cares about. That didn't matter before, but for aggregates we care about the number of results, not just the values. Now I count the number of repeated solutions:

``` julia 
repeats = 1
for buffer_ix in buffer_ixes
  repeats = :($repeats * (his[$buffer_ix] - los[$buffer_ix]))
end
body = :(aggregate = $(aggregate_add)(aggregate, $aggregate_expr, $repeats))
```

The `aggregate_add` is now required to take a third argument that gives an exponent to the operation.

``` julia 
@inline add_exp(a, b, n) = a + (b * n)
@inline mul_exp(a, b, n) = a * (b ^ n)
```

I split the old `plan` into `analyse` and `plan_join` so that I could reuse the parts:

``` julia
function plan_query(returned_variables, typed_variables, aggregate, query)
  aggregate_zero, aggregate_add, aggregate_expr = aggregate
  aggregate_type, variables, variable_types, return_ix = analyse(returned_variables, typed_variables, aggregate)
  join = plan_join(returned_variables, aggregate, aggregate_type, variables, variable_types, return_ix, query)
  project_variables = Any[variable for (variable, variable_type) in zip(variables, variable_types) if variable in returned_variables]
  project_variable_types = Any[variable_type for (variable, variable_type) in zip(variables, variable_types) if variable in returned_variables]
  push!(project_variables, :prev_aggregate)
  push!(project_variable_types, aggregate_type)
  project_aggregate = [aggregate_zero, aggregate_add, :prev_aggregate]
  project_query = quote 
    intermediate($(project_variables...))
  end
  project_return_ix = length(returned_variables) + 1
  project = plan_join(returned_variables, project_aggregate, aggregate_type, project_variables, project_variable_types, project_return_ix, project_query)
  quote 
    let $(esc(:intermediate)) = let; $join; end
      $project
    end
  end
end
```

The default aggregate just counts the number of results:

``` julia 
macro query(returned_variables, typed_variables, query)
  :(@query($returned_variables, $typed_variables, (0, add_exp, 1::Int64), $query))
end

macro query(returned_variables, typed_variables, aggregate, query)
  plan_query(returned_variables.args, typed_variables.args, aggregate.args, query)
end
```

Now we can ask questions like how many times each artist appears on a given playlist:

``` julia 
@query([pn, an],
[pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
begin
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end)
```

I've been putting off dealing with hygiene in the planner, but I spent about an hour on a hygiene bug today so I suppose I should move that up the todo list. 

I also have to do something about caching sorted relations, and then I think I have enough to try the [Join Order Benchmark](http://www.vldb.org/pvldb/vol9/p204-leis.pdf). It uses the IMDB dataset (which is about 3.6GB of strings) and asks questions such as:

``` sql
SELECT MIN(cn1.name) AS first_company,
       MIN(cn2.name) AS second_company,
       MIN(mi_idx1.info) AS first_rating,
       MIN(mi_idx2.info) AS second_rating,
       MIN(t1.title) AS first_movie,
       MIN(t2.title) AS second_movie
FROM company_name AS cn1,
     company_name AS cn2,
     info_type AS it1,
     info_type AS it2,
     kind_type AS kt1,
     kind_type AS kt2,
     link_type AS lt,
     movie_companies AS mc1,
     movie_companies AS mc2,
     movie_info_idx AS mi_idx1,
     movie_info_idx AS mi_idx2,
     movie_link AS ml,
     title AS t1,
     title AS t2
WHERE cn1.country_code != '[us]'
  AND it1.info = 'rating'
  AND it2.info = 'rating'
  AND kt1.kind IN ('tv series',
                   'episode')
  AND kt2.kind IN ('tv series',
                   'episode')
  AND lt.link IN ('sequel',
                  'follows',
                  'followed by')
  AND mi_idx2.info < '3.5'
  AND t2.production_year BETWEEN 2000 AND 2010
  AND lt.id = ml.link_type_id
  AND t1.id = ml.movie_id
  AND t2.id = ml.linked_movie_id
  AND it1.id = mi_idx1.info_type_id
  AND t1.id = mi_idx1.movie_id
  AND kt1.id = t1.kind_id
  AND cn1.id = mc1.company_id
  AND t1.id = mc1.movie_id
  AND ml.movie_id = mi_idx1.movie_id
  AND ml.movie_id = mc1.movie_id
  AND mi_idx1.movie_id = mc1.movie_id
  AND it2.id = mi_idx2.info_type_id
  AND t2.id = mi_idx2.movie_id
  AND kt2.id = t2.kind_id
  AND cn2.id = mc2.company_id
  AND t2.id = mc2.movie_id
  AND ml.linked_movie_id = mi_idx2.movie_id
  AND ml.linked_movie_id = mc2.movie_id
  AND mi_idx2.movie_id = mc2.movie_id;
``` 

Or:

``` sql 
SELECT MIN(mc.note) AS production_note,
       MIN(t.title) AS movie_title,
       MIN(t.production_year) AS movie_year
FROM company_type AS ct,
     info_type AS it,
     movie_companies AS mc,
     movie_info_idx AS mi_idx,
     title AS t
WHERE ct.kind = 'production companies'
  AND it.info = 'top 250 rank'
  AND mc.note NOT LIKE '%(as Metro-Goldwyn-Mayer Pictures)%'
  AND (mc.note LIKE '%(co-production)%'
       OR mc.note LIKE '%(presents)%')
  AND ct.id = mc.company_type_id
  AND t.id = mc.movie_id
  AND t.id = mi_idx.movie_id
  AND mc.movie_id = mi_idx.movie_id
  AND it.id = mi_idx.info_type_id;
```

## 2016 Aug 6

Ok, so we're gonna store the columns in some object that caches various sort orders.

``` julia
type Relation{T <: Tuple} # where T is a tuple of columns
  columns::T
  indexes::Dict{Vector{Int64},T}
end

function Relation{T}(columns::T)
  Relation(columns, Dict{Vector{Int64},T}())
end

function index{T}(relation::Relation{T}, order::Vector{Int64})
  get!(relation.indexes, order) do
    index::T = tuple([(ix in order) ? copy(column) : Vector{eltype(column)}() for (ix, column) in enumerate(relation.columns)]...)
    quicksort!(tuple([index[ix] for ix in order]...))
    index
  end
end
```

We're not being very smart about sharing indexes. If we request [1,2] and there is already an index for [1,2,3] we could just return that, but instead we make a new and pointless index. I'll fix that one day.

When we create a new index, we sort the columns in the order specified but return them in the original order, with any unsorted columns emptied out. This ensures that the return type of the function doesn't depend on the order. Eventually I'll get around to wrapping each query in a function to create a dispatch point and then it won't matter, but for now this helps Julia correctly infer types downstream.

I currently have IMDbPY inserting data into postgres. That reportedly takes about 8 hours (although the hardware described in the readme is anaemic) but as a side-effect it will spit out csv versions of all the tables that I can use in Imp.

One hour later:

```
loading CSV files into the database
 * LOADING CSV FILE imdb/csv/imdb/csv/complete_cast.csv...
ERROR: unable to import CSV file imdb/csv/imdb/csv/complete_cast.csv: could not open file "imdb/csv/imdb/csv/complete_cast.csv" for reading: No such file or directory
```

It created all the csv files just fine and then somehow mangled the filenames before trying to load them. Trying to just run the csv->db step ran into a different set of errors (which I lost by closing the wrong window :), so let's run it again with the row-by-row insert option. 

In the meantime, I tried to load the livejournal dataset into Julia, which caused the atom plugin to blowup:

``` 
/home/jamie/.atom/packages/julia-client/lib/connection/local.coffee:16
RangeError: Invalid string length
    at Socket.<anonymous> (/home/jamie/.atom/packages/julia-client/lib/connection/local.coffee:16:26)
    at emitOne (events.js:77:13)
    at Socket.emit (events.js:169:7)
    at readableAddChunk (_stream_readable.js:146:16)
    at Socket.Readable.push (_stream_readable.js:110:10)
    at TCP.onread (net.js:523:20)
```

Maybe the problem is that it displays relations by printing the entire contents, rather than just showing the head and tail like it does with large arrays. I poked around inside the source code, found the function that controls rendering and added an override for relations:

``` julia
import Atom
function Atom.render(editor::Atom.Editor, relation::Relation)
  Atom.render(editor, relation.columns)
end
```

I checked with a smaller relation that it does affect the rendering. Does it fix the bug? Nope:

```
/home/jamie/.atom/packages/julia-client/lib/connection/local.coffee:16
RangeError: Invalid string length
    at Socket.<anonymous> (/home/jamie/.atom/packages/julia-client/lib/connection/local.coffee:16:26)
    at emitOne (events.js:77:13)
    at Socket.emit (events.js:169:7)
    at readableAddChunk (_stream_readable.js:146:16)
    at Socket.Readable.push (_stream_readable.js:110:10)
    at TCP.onread (net.js:523:20)
```

Debugging by guessing - not a thing.

## 2016 Aug 8

Still working through various problems getting IMDbPY to work.

``` 
ERROR: unable to import CSV file /home/jamie/imdb/csv/movie_link.csv: null value in column "movie_id" violates not-null constraint
DETAIL:  Failing row contains (15021, null, 101237, 12).
CONTEXT:  COPY movie_link, line 15021: "15021,NULL,101237,12"

 * LOADING CSV FILE /home/jamie/imdb/csv/char_name.csv...
# TIME loadCSVFiles() : 6min, 24sec (wall) 0min, 0sec (user) 0min, 0sec (system)
# TIME TOTAL TIME TO INSERT/WRITE DATA : 28min, 42sec (wall) 21min, 52sec (user) 0min, 23sec (system)
building database indexes (this may take a while)
# TIME createIndexes() : 8min, 44sec (wall) 0min, 0sec (user) 0min, 0sec (system)
adding foreign keys (this may take a while)
ERROR caught exception creating a foreign key: insert or update on table "aka_title" violates foreign key constraint "movie_id_exists"
DETAIL:  Key (movie_id)=(0) is not present in table "title".
```

Instead, I found a [link](http://homepages.cwi.nl/~boncz/job/imdb.tgz) to the CSV files the authors of the paper used, and loaded those directly into postgres myself. Which took about 5 minutes.

```
\i job/schema.sql

\copy aka_name from 'imdb/aka_name.csv' csv escape '\'
\copy aka_title from 'imdb/aka_title.csv' csv escape '\'
\copy cast_info from 'imdb/cast_info.csv' csv escape '\'
\copy char_name from 'imdb/char_name.csv' csv escape '\'
\copy comp_cast_type from 'imdb/comp_cast_type.csv' csv escape '\'
\copy company_name from 'imdb/company_name.csv' csv escape '\'
\copy company_type from 'imdb/company_type.csv' csv escape '\'
\copy complete_cast from 'imdb/complete_cast.csv' csv escape '\'
\copy info_type from 'imdb/info_type.csv' csv escape '\'
\copy keyword from 'imdb/keyword.csv' csv escape '\'
\copy kind_type from 'imdb/kind_type.csv' csv escape '\'
\copy link_type from 'imdb/link_type.csv' csv escape '\'
\copy movie_companies from 'imdb/movie_companies.csv' csv escape '\'
\copy movie_info from 'imdb/movie_info.csv' csv escape '\'
\copy movie_info_idx from 'imdb/movie_info_idx.csv' csv escape '\'
\copy movie_keyword from 'imdb/movie_keyword.csv' csv escape '\'
\copy movie_link from 'imdb/movie_link.csv' csv escape '\'
\copy name from 'imdb/name.csv' csv escape '\'
\copy person_info from 'imdb/person_info.csv' csv escape '\'
\copy role_type from 'imdb/role_type.csv' csv escape '\'
\copy title from 'imdb/title.csv' csv escape '\'

\i job/fkindexes.sql
```

Now let's dump the schema in a way that's easy for Imp to read:

```
copy (select table_name, ordinal_position, column_name, data_type from information_schema.columns) to '/home/jamie/imp/data/job_schema.csv' with csv delimiter ',';
```

Eugh, and the csv files themselves have backslash-escaped strings that Julia can't read, so let's re-export those.

```
\copy aka_name to 'job/aka_name.csv' csv escape '"'
\copy aka_title to 'job/aka_title.csv' csv escape '"'
\copy cast_info to 'job/cast_info.csv' csv escape '"'
\copy char_name to 'job/char_name.csv' csv escape '"'
\copy comp_cast_type to 'job/comp_cast_type.csv' csv escape '"'
\copy company_name to 'job/company_name.csv' csv escape '"'
\copy company_type to 'job/company_type.csv' csv escape '"'
\copy complete_cast to 'job/complete_cast.csv' csv escape '"'
\copy info_type to 'job/info_type.csv' csv escape '"'
\copy keyword to 'job/keyword.csv' csv escape '"'
\copy kind_type to 'job/kind_type.csv' csv escape '"'
\copy link_type to 'job/link_type.csv' csv escape '"'
\copy movie_companies to 'job/movie_companies.csv' csv escape '"'
\copy movie_info to 'job/movie_info.csv' csv escape '"'
\copy movie_info_idx to 'job/movie_info_idx.csv' csv escape '"'
\copy movie_keyword to 'job/movie_keyword.csv' csv escape '"'
\copy movie_link to 'job/movie_link.csv' csv escape '"'
\copy name to 'job/name.csv' csv escape '"'
\copy person_info to 'job/person_info.csv' csv escape '"'
\copy role_type to 'job/role_type.csv' csv escape '"'
\copy title to 'job/title.csv' csv escape '"' 
```

Let's grab the first query from the benchmark and get a feel for long it takes.

```
postgres=# prepare q1a as SELECT MIN(mc.note) AS production_note, MIN(t.title) AS movie_title, MIN(t.production_year) AS movie_year FROM company_type AS ct, info_type AS it, movie_companies AS mc, movie_info_idx AS mi_idx, title AS t WHERE ct.kind = 'production companies' AND it.info = 'top 250 rank' AND mc.note  not like '%(as Metro-Goldwyn-Mayer Pictures)%' and (mc.note like '%(co-production)%' or mc.note like '%(presents)%') AND ct.id = mc.company_type_id AND t.id = mc.movie_id AND t.id = mi_idx.movie_id AND mc.movie_id = mi_idx.movie_id AND it.id = mi_idx.info_type_id;
ERROR:  prepared statement "q1a" already exists
Time: 0.356 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.213 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.578 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.109 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.317 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.187 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 5.794 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 5.536 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 5.981 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.122 ms
```

So around 6ms. 

```
postgres=# EXPLAIN ANALYZE execute q1a;
                                                                                 QUERY PLAN                                                                                 
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 Aggregate  (cost=30010.08..30010.09 rows=1 width=45) (actual time=5.704..5.704 rows=1 loops=1)
   ->  Nested Loop  (cost=6482.03..30010.06 rows=3 width=45) (actual time=0.098..5.658 rows=142 loops=1)
         Join Filter: (mc.movie_id = t.id)
         ->  Hash Join  (cost=6481.60..30008.29 rows=3 width=32) (actual time=0.092..5.225 rows=142 loops=1)
               Hash Cond: (mc.company_type_id = ct.id)
               ->  Nested Loop  (cost=6462.68..29987.21 rows=566 width=36) (actual time=0.071..5.173 rows=147 loops=1)
                     ->  Nested Loop  (cost=6462.25..22168.36 rows=12213 width=4) (actual time=0.036..0.091 rows=250 loops=1)
                           ->  Seq Scan on info_type it  (cost=0.00..2.41 rows=1 width=4) (actual time=0.012..0.013 rows=1 loops=1)
                                 Filter: ((info)::text = 'top 250 rank'::text)
                                 Rows Removed by Filter: 112
                           ->  Bitmap Heap Scan on movie_info_idx mi_idx  (cost=6462.25..18715.86 rows=345009 width=8) (actual time=0.022..0.036 rows=250 loops=1)
                                 Recheck Cond: (info_type_id = it.id)
                                 Heap Blocks: exact=2
                                 ->  Bitmap Index Scan on info_type_id_movie_info_idx  (cost=0.00..6375.99 rows=345009 width=0) (actual time=0.015..0.015 rows=250 loops=1)
                                       Index Cond: (info_type_id = it.id)
                     ->  Index Scan using movie_id_movie_companies on movie_companies mc  (cost=0.43..0.63 rows=1 width=32) (actual time=0.020..0.020 rows=1 loops=250)
                           Index Cond: (movie_id = mi_idx.movie_id)
                           Filter: ((note !~~ '%(as Metro-Goldwyn-Mayer Pictures)%'::text) AND ((note ~~ '%(co-production)%'::text) OR (note ~~ '%(presents)%'::text)))
                           Rows Removed by Filter: 33
               ->  Hash  (cost=18.88..18.88 rows=4 width=4) (actual time=0.017..0.017 rows=1 loops=1)
                     Buckets: 1024  Batches: 1  Memory Usage: 9kB
                     ->  Seq Scan on company_type ct  (cost=0.00..18.88 rows=4 width=4) (actual time=0.011..0.011 rows=1 loops=1)
                           Filter: ((kind)::text = 'production companies'::text)
                           Rows Removed by Filter: 3
         ->  Index Scan using title_pkey on title t  (cost=0.43..0.58 rows=1 width=25) (actual time=0.003..0.003 rows=1 loops=142)
               Index Cond: (id = mi_idx.movie_id)
 Execution time: 5.757 ms
```

The overwhelming majority of the time is attributed to the final aggregate, which is weird. I don't know much about how it calculates these times, but I would expect producing the data to take at lesat as much time as reducing it.

Let's get some data into Imp!

``` julia
function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  tables = Dict()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(tables, table_name, []), (ix, column_name, column_type))
  end
  relations = []
  names = []
  for (table_name, columns) in tables
    if isfile("../job/$(table_name).csv")
      rows = readdlm(open("../job/$(table_name).csv"), ',', header=false, quotes=true, comments=false)
      n = size(rows)[1]
      ids = Int64[rows[r,1] for r in 1:n]
      push!(names, symbol(table_name))
      push!(relations, Relation((ids,)))
      for (ix, column_name, column_type) in columns[2:end]
        @show table_name ix column_name column_type
        if column_type == "integer"
          ids = Int64[]
          column = Int64[]
          for r in 1:n
            if !(rows[r, ix] in ("", "null", "NULL"))
              push!(ids, rows[r, 1])
              push!(column, rows[r, ix])
            end
          end
        else
          ids = Int64[]
          column = String[]
          for r in 1:n
            if !(rows[r, ix] in ("", "null", "NULL"))
              push!(ids, rows[r, 1])
              push!(column, string(rows[r, ix]))
            end
          end
        end
        push!(names, symbol(table_name, "_", column_name))
        push!(relations, Relation((ids, column)))
      end
    end
  end
  (names, relations)
end
```

This reads the schema I dumped out of postgres and builds a normalized set of relations (taking advantage of the fact that every table in the dataset has a single integer as it's primary key). I'm normalizing it this way to avoid having to represent with null entries directly. Possible future feature.

I'm using the stdlib csv reading function, which generates a single big array containing all the data, meaning that if there are any strings then all the integers have to be boxed too and everything goes to poop.  

The csv reading code also returns `SubString`s - pointers to slices of the mmaped file - rather than allocating individual strings. But this seems unrealistic - I don't actually expect real-world data to arrive all in one nice contiguous file. So I'm reallocating them all as individual strings.

All of this means that loading the data takes FOREVER, but the final representation is pretty sensible. Later I'll have to find a faster way of doing this. Maybe DataFrames.jl is better?

``` julia
using DataFrames

function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  table_column_names = Dict()
  table_column_types = Dict()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), (column_type == "integer" ? Int64 : String))
  end
  relations = []
  names = []
  for (table_name, column_names) in table_column_names
    if isfile("../job/$(table_name).csv")
      column_types = table_column_types[table_name]
      @show table_name column_names column_types
      frame = readtable(open("../imdb/$(table_name).csv"), header=false, eltypes=column_types)
      n = length(frame[1])
      ids = copy(frame[1].data)
      for (ix, (column_name, column_type)) in enumerate(zip(column_names, column_types))
        @show table_name ix column_name column_type
        data_array = frame[ix]
        if ix == 1
          push!(names, symbol(table_name))
          push!(relations, Relation((ids,)))
        else
          column_ids = Int64[id for (ix, id) in enumerate(ids) if !(data_array.na[ix])]
          local column
          if isa(data_array, DataArray{Int64})
            let data::Vector{Int64} = data_array.data
              column = Int64[d for (ix, d) in enumerate(data) if !(data_array.na[ix])]
            end
          elseif isa(data_array, DataArray{String})
            let data::Vector{String} = data_array.data
              column = String[d for (ix, d) in enumerate(data_array.data) if !(data_array.na[ix])]
            end
          end
          push!(names, symbol(table_name, "_", column_name))
          push!(relations, Relation((column_ids, column)))
        end
      end
    end
  end
  (names, relations)
end
```

Woah, way way faster. 

Weirdly, unpacking the results into individual variable names blows up with an out-of-memory error.

``` julia 
person_info,person_info_person_id,person_info_info_type_id,person_info_info,person_info_note,title,title_title,title_imdb_index,title_kind_id,title_production_year,title_imdb_id,title_phonetic_code,title_episode_of_id,title_season_nr,title_episode_nr,title_series_years,title_md5sum,link_type,link_type_link,cast_info,cast_info_person_id,cast_info_movie_id,cast_info_person_role_id,cast_info_note,cast_info_nr_order,cast_info_role_id,movie_info_idx,movie_info_idx_movie_id,movie_info_idx_info_type_id,movie_info_idx_info,movie_info_idx_note,name,name_name,name_imdb_index,name_imdb_id,name_gender,name_name_pcode_cf,name_name_pcode_nf,name_surname_pcode,name_md5sum,info_type,info_type_info,aka_name,aka_name_person_id,aka_name_name,aka_name_imdb_index,aka_name_name_pcode_cf,aka_name_name_pcode_nf,aka_name_surname_pcode,aka_name_md5sum,movie_info,movie_info_movie_id,movie_info_info_type_id,movie_info_info,movie_info_note,role_type,role_type_role,aka_title,aka_title_movie_id,aka_title_title,aka_title_imdb_index,aka_title_kind_id,aka_title_production_year,aka_title_phonetic_code,aka_title_episode_of_id,aka_title_season_nr,aka_title_episode_nr,aka_title_note,aka_title_md5sum,complete_cast,complete_cast_movie_id,complete_cast_subject_id,complete_cast_status_id,movie_keyword,movie_keyword_movie_id,movie_keyword_keyword_id,kind_type,kind_type_kind,movie_link,movie_link_movie_id,movie_link_linked_movie_id,movie_link_link_type_id,company_name,company_name_name,company_name_country_code,company_name_imdb_id,company_name_name_pcode_nf,company_name_name_pcode_sf,company_name_md5sum,keyword,keyword_keyword,keyword_phonetic_code,comp_cast_type,comp_cast_type_kind,char_name,char_name_name,char_name_imdb_index,char_name_imdb_id,char_name_name_pcode_nf,char_name_surname_pcode,char_name_md5sum,movie_companies,movie_companies_movie_id,movie_companies_company_id,movie_companies_company_type_id,movie_companies_note,company_type,company_type_kind = relations
```

I have no idea why. At a guess, unpacking 100-odd variables at once triggers some weird corner case in the compiler.

But now the julia process is dead and I have to load all that data into memory again. Sigh...

The reason I wanted to unpack everything is that the query compiler currently can't handle non-symbol relation names eg `person_info_person_id(p, pi)` works but `db[:person_info, :person_id](p, pi)` does not. But I can fix that pretty easily - let's finally get around to wrapping the query in a function.

``` julia
function plan(...)
  ...
  quote 
    # TODO pass through any external vars too to avoid closure boxing grossness
    function query($([symbol("relation_", clause) for clause in relation_clauses]...))
      $setup
      $body
      Relation(tuple($([symbol("results_", variable) for variable in returned_variables]...), results_aggregate))
    end
    query($([esc(query.args[clause].args[1]) for clause in relation_clauses]...))
  end
end
```

So the generated code will look like:

``` julia 
function query(relation_1, ...)
  ...
end
query(db[:person_info, :person_id], ...)
```

Now I'll load the imdb data into a dict of relations, and then try to serialize it so I don't have to do it again:

``` julia 
job = @time read_job()

open("../job/imp.bin", "w") do f
    @time serialize(f, job)
end

# 743.572276 seconds (4.92 G allocations: 139.663 GB, 65.42% gc time)
# 42.551220 seconds (92.15 M allocations: 1.384 GB, 7.74% gc time)
```

140GB of temporary allocations. Something in there is still a mess.

``` julia 
job = @time deserialize(open("../job/imp.bin"))
# 700.359796 seconds (943.00 M allocations: 50.969 GB, 78.30% gc time)
# OutOfMemoryError()
```

So that's weird. It made a big mess of allocations deserializing the data, finished, then about 3 seconds later threw an out of memory error. 

Later, trying to rebuild the dataset, Julia dies with:

```
Julia has stopped: null, SIGKILL
```

This is frustrating. Reading a 6gb dataset on a machine with 32gb of ram should not be difficult.

After several attempts, JLD manages to both save and load the dataset without exploding, although never both sequentially. After gc, top shows:

```
13908 jamie     20   0 22.560g 0.017t  64752 S  61.4 54.2   2:03.90 julia
```

I asked to see the results (bearing in mind that the representation is truncated) and...

``` 
OutOfMemoryError()
 in resize!(::Array{UInt8,1}, ::UInt64) at ./array.jl:470
 in ensureroom at ./iobuffer.jl:194 [inlined]
 in unsafe_write(::Base.AbstractIOBuffer{Array{UInt8,1}}, ::Ptr{UInt8}, ::UInt64) at ./iobuffer.jl:275
 in write(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{UInt8,1}) at ./io.jl:161
 in show at ./show.jl:234 [inlined]
 in show_delim_array(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{Int64,1}, ::String, ::String, ::String, ::Bool, ::Int64, ::Int64) at ./show.jl:318
 in show_delim_array(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{Int64,1}, ::String, ::String, ::String, ::Bool) at ./show.jl:300
 in show_vector(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{Int64,1}, ::String, ::String) at ./show.jl:1666
 in #showarray#252(::Bool, ::Function, ::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{Int64,1}, ::Bool) at ./show.jl:1585
 in show_delim_array(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Tuple{Array{Int64,1},Array{Int64,1}}, ::Char, ::Char, ::Char, ::Bool, ::Int64, ::Int64) at ./show.jl:355
 in show(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Tuple{Array{Int64,1},Array{Int64,1}}) at ./show.jl:376
 in show_default(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Any) at ./show.jl:130
 in show(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Any) at ./show.jl:116
 in show(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::MIME{Symbol("text/plain")}, ::Dict{Any,Any}) at ./replutil.jl:94
 in verbose_show(::Base.AbstractIOBuffer{Array{UInt8,1}}, ::MIME{Symbol("text/plain")}, ::Dict{Any,Any}) at ./multimedia.jl:50
 in #sprint#226(::Void, ::Function, ::Int64, ::Function, ::MIME{Symbol("text/plain")}, ::Vararg{Any,N}) at ./strings/io.jl:37
 in Type at /home/jamie/.julia/v0.5/Atom/src/display/view.jl:78 [inlined]
 in Type at /home/jamie/.julia/v0.5/Atom/src/display/view.jl:79 [inlined]
 in render(::Atom.Editor, ::Dict{Any,Any}) at /home/jamie/.julia/v0.5/Atom/src/display/display.jl:23
 in (::Atom.##91#95)(::Dict{String,Any}) at /home/jamie/.julia/v0.5/Atom/src/eval.jl:62
 in handlemsg(::Dict{String,Any}, ::Dict{String,Any}, ::Vararg{Dict{String,Any},N}) at /home/jamie/.julia/v0.5/Atom/src/comm.jl:71
 in (::Atom.##5#8)() at ./event.jl:46
```

But if I treat it with kid gloves and never ask to see the actual result, I can write my first tiny query against the dataset:

``` julia
@query([cid, cn],
[cid::Int64, cn::String],
begin 
  job["company_name", "name"](cid, cn)
end)
```

That works fine.

But if I wrap it in a function and run the function I get a bounds error (which takes a long time to generate because Julia prints the entire relation in the error). I think inside a function scope, functions are all defined up top, but globally the definitions are executed sequentially. So if the function names collide, behaviour in each scope is different. I added a counter just uniquefies each function name and the problem went away.

Let's have a go at query 1a.

``` julia 
# SELECT MIN(mc.note) AS production_note,
#        MIN(t.title) AS movie_title,
#        MIN(t.production_year) AS movie_year
# FROM company_type AS ct,
#      info_type AS it,
#      movie_companies AS mc,
#      movie_info_idx AS mi_idx,
#      title AS t
# WHERE ct.kind = 'production companies'
#   AND it.info = 'top 250 rank'
#   AND mc.note NOT LIKE '%(as Metro-Goldwyn-Mayer Pictures)%'
#   AND (mc.note LIKE '%(co-production)%'
#        OR mc.note LIKE '%(presents)%')
#   AND ct.id = mc.company_type_id
#   AND t.id = mc.movie_id
#   AND t.id = mi_idx.movie_id
#   AND mc.movie_id = mi_idx.movie_id
#   AND it.id = mi_idx.info_type_id;

function f()
  @query([],
  [ct_kind::String, ct_id::Int64, mc_id::Int64, mc_note::String, t_id::Int64, mii_id::Int64, it_id::Int64, it_info::String, t_production_year::Int64],
  (3000, min_exp, t_production_year),
  begin 
    ct_kind = "production companies"
    it_info = "top 250 rank"
    job["company_type", "kind"](ct_id, ct_kind)
    job["info_type", "info"](it_id, it_info)
    job["movie_companies", "note"](mc_id, mc_note)
    ismatch(r".*as Metro-Goldwyn-Mayer Pictures.*", mc_note) == false
    (ismatch(r".*co-production.*", mc_note) || ismatch(r".*presents.*", mc_note)) == true
    job["movie_companies", "company_type_id"](mc_id, ct_id)
    job["title", "production_year"](t_id, t_production_year)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_info_idx", "movie_id"](mii_id, t_id)
    job["movie_info_idx", "info_type_id"](mii_id, it_id)
  end)
end

# first run (indexing + compilation)
# 7.278131 seconds (479.39 k allocations: 192.071 MB, 82.77% gc time)

# second run
# 0.118113 seconds (292.96 k allocations: 4.476 MB)
```

118ms. Not going to knock postgres off any pedastals just yet. 

I want to know what's going on with those allocations. There should barely be any. I squelched a few type-inference failures but it didn't change the number of allocations at all, which is weird.

## 2016 Aug 09

Looks like `ismatch` causes a single heap allocation on each call. So nothing wrong with my compiler.

Let's pick a query with no regexes.

``` julia
# SELECT MIN(t.title) AS movie_title
# FROM company_name AS cn,
#      keyword AS k,
#      movie_companies AS mc,
#      movie_keyword AS mk,
#      title AS t
# WHERE cn.country_code ='[de]'
#   AND k.keyword ='character-name-in-title'
#   AND cn.id = mc.company_id
#   AND mc.movie_id = t.id
#   AND t.id = mk.movie_id
#   AND mk.keyword_id = k.id
#   AND mc.movie_id = mk.movie_id;

function q2a()
  @query([],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64, mc_id::Int64, cn_id::Int64, de::String, title::String],
  ("zzzzzzzzzzz", min_exp, title::String),
  begin
    de = "[de]"
    job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    job["movie_companies", "company_id"](mc_id, cn_id)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, title)
  end)
end

@time q2a()

# 1.998575 seconds (142.43 k allocations: 44.412 MB, 63.25% gc time)
# 0.126513 seconds (108 allocations: 7.250 KB)
# 0.125623 seconds (108 allocations: 7.250 KB)
```

``` 
postgres=# execute q2a;
       movie_title        
--------------------------
 008 - Agent wider Willen
(1 row)

Time: 2388.770 ms
postgres=# execute q2a;
       movie_title        
--------------------------
 008 - Agent wider Willen
(1 row)

Time: 449.339 ms
postgres=# execute q2a;
       movie_title        
--------------------------
 008 - Agent wider Willen
(1 row)

Time: 449.340 ms
```

Not worth reading too much into that, because I'm getting a different answer to postgres.

Man, where do I even start debugging something like that. I guess, break the query down into pieces and find where it starts to diverge.

``` julia
function q2a()
  @query([cnit, k_id],
  [cnit::String, k_id::Int64], #, mk_id::Int64, t_id::Int64, mc_id::Int64, cn_id::Int64, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    # job["movie_companies", "company_id"](mc_id, cn_id)
    # job["movie_companies", "movie_id"](mc_id, t_id)
    # job["movie_keyword", "movie_id"](mk_id, t_id)
    # job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

function q2a()
  @query([mk_id],
  [cnit::String, k_id::Int64, mk_id::Int64], # t_id::Int64, mc_id::Int64, cn_id::Int64, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    # job["movie_companies", "company_id"](mc_id, cn_id)
    # job["movie_companies", "movie_id"](mc_id, t_id)
    # job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

function q2a()
  @query([t_id],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64], #, mc_id::Int64, cn_id::Int64, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    # job["movie_companies", "company_id"](mc_id, cn_id)
    # job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

function q2a()
  @query([mc_id],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64, mc_id::Int64], # cn_id::Int64, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    # job["movie_companies", "company_id"](mc_id, cn_id)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

function q2a()
  @query([cn_id],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64, mc_id::Int64, cn_id::Int64], #, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    job["movie_companies", "company_id"](mc_id, cn_id)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

function q2a()
  @query([title],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64, mc_id::Int64, cn_id::Int64, de::String, title::String],
  begin
    de = "[de]"
    job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    job["movie_companies", "company_id"](mc_id, cn_id)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, title)
  end)
end
```

Eugh, that was tedious. Turns out that the query is correct, but `min` in sql uses a different ordering and I forget to use `count(distinct ...)` when I double-checked the total results.

So let's have sql return the distinct count and Imp return the column of results, which seems roughly fair.

```
postgres=# execute q2a_all;
Time: 443.249 ms
postgres=# prepare q2a_distinct as select count(distinct t.title) AS movie_title FROM company_name AS cn, keyword AS k, movie_companies AS mc, movie_keyword AS mk, title AS t WHERE cn.country_code ='[de]' AND k.keyword ='character-name-in-title' AND cn.id = mc.company_id AND mc.movie_id = t.id AND t.id = mk.movie_id AND mk.keyword_id = k.id AND mc.movie_id = mk.movie_id;
PREPARE
Time: 0.468 ms
postgres=# execute q2a_distinct
postgres-# ;
 movie_title 
-------------
        4127
(1 row)

Time: 455.719 ms
postgres=# execute q2a_distinct;
 movie_title 
-------------
        4127
(1 row)

Time: 450.318 ms
postgres=# execute q2a_distinct;
 movie_title 
-------------
        4127
(1 row)

Time: 441.992 ms
```

``` julia
function q2a()
  @query([title],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64, mc_id::Int64, cn_id::Int64, de::String, title::String],
  begin
    de = "[de]"
    job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    job["movie_companies", "company_id"](mc_id, cn_id)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, title)
  end)
end

@time q2a()

# 0.128545 seconds (197 allocations: 646.297 KB)
# 0.140465 seconds (197 allocations: 646.297 KB)
# 0.138893 seconds (197 allocations: 646.297 KB)
```

Score. Faster than postgres on q2a, with the first variable ordering I tried.

Let's go back to q1a and steal the execution plan from postgres.

```
postgres=# prepare q1a_distinct as SELECT count(distinct t.production_year) AS movie_year FROM company_type AS ct, info_type AS it, movie_companies AS mc, movie_info_idx AS mi_idx, title AS t WHERE ct.kind = 'production companies' AND it.info = 'top 250 rank' AND mc.note  not like '%(as Metro-Goldwyn-Mayer Pictures)%' and (mc.note like '%(co-production)%' or mc.note like '%(presents)%') AND ct.id = mc.company_type_id AND t.id = mc.movie_id AND t.id = mi_idx.movie_id AND mc.movie_id = mi_idx.movie_id AND it.id = mi_idx.info_type_id;
ERROR:  prepared statement "q1a_distinct" already exists
Time: 0.715 ms
postgres=# explain analyze execute q1a_distinct;
                                                                                 QUERY PLAN                                                                                 
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 Aggregate  (cost=30010.06..30010.07 rows=1 width=4) (actual time=9.987..9.987 rows=1 loops=1)
   ->  Nested Loop  (cost=6482.03..30010.06 rows=3 width=4) (actual time=0.314..9.934 rows=142 loops=1)
         Join Filter: (mc.movie_id = t.id)
         ->  Hash Join  (cost=6481.60..30008.29 rows=3 width=8) (actual time=0.302..9.292 rows=142 loops=1)
               Hash Cond: (mc.company_type_id = ct.id)
               ->  Nested Loop  (cost=6462.68..29987.21 rows=566 width=12) (actual time=0.266..9.193 rows=147 loops=1)
                     ->  Nested Loop  (cost=6462.25..22168.36 rows=12213 width=4) (actual time=0.137..0.243 rows=250 loops=1)
                           ->  Seq Scan on info_type it  (cost=0.00..2.41 rows=1 width=4) (actual time=0.031..0.033 rows=1 loops=1)
                                 Filter: ((info)::text = 'top 250 rank'::text)
                                 Rows Removed by Filter: 112
                           ->  Bitmap Heap Scan on movie_info_idx mi_idx  (cost=6462.25..18715.86 rows=345009 width=8) (actual time=0.100..0.160 rows=250 loops=1)
                                 Recheck Cond: (info_type_id = it.id)
                                 Heap Blocks: exact=2
                                 ->  Bitmap Index Scan on info_type_id_movie_info_idx  (cost=0.00..6375.99 rows=345009 width=0) (actual time=0.070..0.070 rows=250 loops=1)
                                       Index Cond: (info_type_id = it.id)
                     ->  Index Scan using movie_id_movie_companies on movie_companies mc  (cost=0.43..0.63 rows=1 width=8) (actual time=0.035..0.035 rows=1 loops=250)
                           Index Cond: (movie_id = mi_idx.movie_id)
                           Filter: ((note !~~ '%(as Metro-Goldwyn-Mayer Pictures)%'::text) AND ((note ~~ '%(co-production)%'::text) OR (note ~~ '%(presents)%'::text)))
                           Rows Removed by Filter: 33
               ->  Hash  (cost=18.88..18.88 rows=4 width=4) (actual time=0.023..0.023 rows=1 loops=1)
                     Buckets: 1024  Batches: 1  Memory Usage: 9kB
                     ->  Seq Scan on company_type ct  (cost=0.00..18.88 rows=4 width=4) (actual time=0.017..0.019 rows=1 loops=1)
                           Filter: ((kind)::text = 'production companies'::text)
                           Rows Removed by Filter: 3
         ->  Index Scan using title_pkey on title t  (cost=0.43..0.58 rows=1 width=8) (actual time=0.004..0.004 rows=1 loops=142)
               Index Cond: (id = mi_idx.movie_id)
 Execution time: 10.158 ms
(27 rows)

Time: 10.551 ms
postgres=# execute q1a_distinct;
 movie_year 
------------
         57
(1 row)

Time: 20.732 ms
postgres=# execute q1a_distinct;
 movie_year 
------------
         57
(1 row)

Time: 18.280 ms
```

The execution plan is a bit bushy so I can't copy it perfectly without caching or factorisation, but I can approximate it with this ordering.

``` julia 
function q1a()
  @query([t_production_year],
  [it_info::String, it_id::Int64, mii_id::Int64, t_id::Int64, ct_id::Int64, ct_kind::String, mc_id::Int64, mc_note::String, t_production_year::Int64],
  begin 
    ct_kind = "production companies"
    it_info = "top 250 rank"
    job["company_type", "kind"](ct_id, ct_kind)
    job["info_type", "info"](it_id, it_info)
    job["movie_companies", "note"](mc_id, mc_note)
    ismatch(r".*as Metro-Goldwyn-Mayer Pictures.*", mc_note) == false
    (ismatch(r".*co-production.*", mc_note) || ismatch(r".*presents.*", mc_note)) == true
    job["movie_companies", "company_type_id"](mc_id, ct_id)
    job["title", "production_year"](t_id, t_production_year)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_info_idx", "movie_id"](mii_id, t_id)
    job["movie_info_idx", "info_type_id"](mii_id, it_id)
  end)
end

@time q1a()

# 0.004359 seconds (1.05 k allocations: 35.516 KB)
# 0.002895 seconds (1.05 k allocations: 35.516 KB)
# 0.003321 seconds (1.05 k allocations: 35.516 KB)
```

So my code is fine, I'm just a crappy query planner.

I only had an hour or two to work on this today, but I'm glad I got to see some exciting numbers.

## 2016 Aug 10

So here is q3a:

``` julia

function q3a()
  # "Denish" is in original too
  mi_infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query([t_title],
  [mi_info::String, mi_id::Int64, k_keyword::String, k_id::Int64, mk_id::Int64, t_id::Int64, t_production_year::Int64, t_title::String],
  begin 
    job["keyword", "keyword"](k_id, k_keyword)
    contains(k_keyword, "sequel") == true
    job["movie_info", "info"](mi_id, mi_info)
    (mi_info in mi_infos) == true
    job["title", "production_year"](t_id, t_production_year)
    t_production_year > 2005
    job["movie_info", "movie_id"](mi_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, t_title)
  end)
end
```

It touches `movie_info` which is one of the biggest tables in the dataset at ~15m rows. This takes forever to index, so long that I haven't succesfully waited it out yet.

(I have to restart Julia if something gets stuck in a loop or is taking too long, which means reloading the IMDB dataset. Sometimes Atom gets stuck and can't restart Julia, so I have to restart Atom too. Sometimes Atom forgets my project settings, so I have to reopen and reorganize all the files I'm working with. This costs me a significant proportion of the day and the endless context switches are a huge problem. What can I improve?)

But if I index a similarly-sized relation full of random integers:

``` julia 
edge = Relation((rand(1:Int64(1E5), Int64(15E6)), rand(1:Int64(1E5), Int64(15E6))))
@time index(edge, [1,2])
# 2.178177 seconds (146.18 k allocations: 235.030 MB, 0.84% gc time)
```

Even if I put in a string column and force it so touch all the strings:

``` julia
edge = Relation(([1 for _ in 1:Int64(15E6)], String[string(i) for i in rand(1:Int64(1E5), Int64(15E6))]))
@time index(edge, [1,2])
# 17.183060 seconds (59 allocations: 228.885 MB)
```

Sorting the text version of movie_info at the terminal works ok:

``` 
jamie@machine:~$ time sort job/movie_info.csv > /dev/null

real	0m8.972s
user	0m30.316s
sys	0m4.148s
```

So what's the deal? Why does this take seconds when `movie_info` takes hours or more? 

Maybe there's a bug in my quicksort? Let's print the lo/hi for each recursive call and see if it's getting stuck somewhere.

Eugh, atom crashed. Maybe let's print to a file then.

```
...
14431187 14431188
14431184 14431185
14431181 14431182
14431178 14431179
14431175 14431176
14431172 14431173
14431169 14431170
14431166 14431167
14431163 14431164
14431160 14431161
14431157 14431158
14431154 14431155
14431151 14431152
14431148 14431149
14431145 14431146
14431142 14431143
14431139 14431140
14431136 14431137
14431133 14431134
14431130 14431131
14431127 14431128
14431124 14431125
14431121 14431122
14431118 14431119
14431115 14431116
14431112 14431113
...
```

Look at that, recursive calls to `quicksort!` on a bunch of single element subarrays, spaced out by exactly 3 each time. Something funky is going on.

Let's look at the function I copied from the stdlib. There is some weirdness in here where it sorts the smallest partition first and then recurs on the larger partition.

``` julia 
function quicksort!($(cs...), lo::Int, hi::Int)
  write(test, string(lo, " ", hi, "\n"))
  @inbounds while lo < hi
    if hi-lo <= 20 
      insertion_sort!($(cs...), lo, hi)
      return 
    end
    j = partition!($(cs...), lo, hi)
    if j-lo < hi-j
      lo < (j-1) && quicksort!($(cs...), lo, j-1)
      lo = j+1
    else
      j+1 < hi && quicksort!($(cs...), j+1, hi)
      hi = j-1
    end
  end
  return
end
```

It also does something funky to get lo/mid/hi in the right order before partitioning:

``` julia
@inline function select_pivot!($(cs...), lo::Int, hi::Int)
  @inbounds begin
    mi = (lo+hi)>>>1
    if lt($(cs...), mi, lo)
      swap2($(cs...), lo, mi)
    end
    if lt($(cs...), hi, mi)
      if lt($(cs...), hi, lo)
        swap3($(cs...), lo, mi, hi)
      else
        swap2($(cs...), mi, hi)
      end
    end
    swap2($(cs...), lo, mi)
  end
  return lo
end
```

Let's try just picking pivots at random.

``` julia 
function partition!($(cs...), lo::Int, hi::Int)
  @inbounds begin
    pivot = rand(lo:hi)
    swap2($(cs...), pivot, lo)
    i, j = lo+1, hi
    while true
      while lt($(cs...), i, lo); i += 1; end;
      while lt($(cs...), lo, j); j -= 1; end;
      i >= j && break
      swap2($(cs...), i, j)
      i += 1; j -= 1
    end
    swap2($(cs...), lo, j)
    return j
  end
end

function quicksort!($(cs...), lo::Int, hi::Int)
  @inbounds if hi-lo <= 0
    return
  elseif hi-lo <= 20 
    insertion_sort!($(cs...), lo, hi)
  else
    j = partition!($(cs...), lo, hi)
    quicksort!($(cs...), lo, j-1)
    quicksort!($(cs...), j+1, hi)
  end
end
```

Not totally sure that's correct, but I haven't found any mis-sorts so far. 

Sorting becomes slightly slower, maybe around 10%, not enough to make me care, because:

``` julia 
@time index(job["movie_info", "info"], [1,2])
# 1.450726 seconds (210.51 k allocations: 235.458 MB)
```

``` julia
function q3a()
  # "Denish" is in original too
  mi_infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query([t_title],
  [k_keyword::String, k_id::Int64, mk_id::Int64, t_id::Int64, t_title::String, t_production_year::Int64, mi_id::Int64, mi_info::String],
  begin 
    job["keyword", "keyword"](k_id, k_keyword)
    contains(k_keyword, "sequel") == true
    job["movie_info", "info"](mi_id, mi_info)
    (mi_info in mi_infos) == true
    job["title", "production_year"](t_id, t_production_year)
    t_production_year > 2005
    job["movie_info", "movie_id"](mi_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, t_title)
  end)
end

# Job.q3a x1 (+compilation +indexing)
#  5.088275 seconds (545.92 k allocations: 692.371 MB)
# Job.q3a x20
#  2.178364 seconds (3.60 k allocations: 510.625 KB)
```

```
postgres=# prepare q3a_distinct as SELECT count(distinct t.title) AS movie_title FROM keyword AS k, movie_info AS mi, movie_keyword AS mk, title AS t WHERE k.keyword  like '%sequel%' AND mi.info  IN ('Sweden', 'Norway', 'Germany', 'Denmark', 'Swedish', 'Denish', 'Norwegian', 'German') AND t.production_year > 2005 AND t.id = mi.movie_id AND t.id = mk.movie_id AND mk.movie_id = mi.movie_id AND k.id = mk.keyword_id;
PREPARE
Time: 5.955 ms
postgres=# execute q3a_distinct;
 movie_title 
-------------
         105
(1 row)

Time: 2596.093 ms
postgres=# execute q3a_distinct;
 movie_title 
-------------
         105
(1 row)

Time: 220.938 ms
postgres=# execute q3a_distinct;
 movie_title 
-------------
         105
(1 row)

Time: 187.519 ms
postgres=# execute q3a_distinct;
 movie_title 
-------------
         105
(1 row)

Time: 177.598 ms
```

About 2x faster than postgres on this one. Imp is a bit handicapped atm because it can't turn that `in` into a join, and instead gets stuck with a table scan. Should be an easy optimisation to add though.

I wrote q4a early while waiting on q3a, so let's try that too.

``` julia 
function q4a()
  @query([mii_info],
  [k_keyword::String, k_id::Int64, mk_id::Int64, t_id::Int64, t_production_year::Int64, it_info::String, it_id::Int64, mii_id::Int64, mii_info::String],
  begin
    job["info_type", "info"](it_id, it_info)
    it_info = "rating"
    job["keyword", "keyword"](k_id, k_keyword)
    contains(k_keyword, "sequel") == true
    job["movie_info_idx", "info"](mii_id, mii_info)
    mii_info > "5.0"
    job["title", "production_year"](t_id, t_production_year)
    t_production_year > 2005
    job["movie_info_idx", "movie_id"](mii_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, t_title)
    job["movie_info_idx", "info_type_id"](mii_id, it_id)
    job["movie_info_idx", "movie_id"](mii_id, t_id)
  end)
end

# Job.q4a x1 (+compilation +indexing)
#   0.552385 seconds (63.57 k allocations: 43.060 MB)
# Job.q4a x100
#   5.834656 seconds (24.30 k allocations: 5.669 MB)
```

```
postgres=# prepare q4a_distinct as SELECT count(distinct mi_idx.info) AS rating, MIN(t.title) AS movie_title FROM info_type AS it, keyword AS k, movie_info_idx AS mi_idx, movie_keyword AS mk, title AS t WHERE it.info ='rating' AND k.keyword  like '%sequel%' AND mi_idx.info  > '5.0' AND t.production_year > 2005 AND t.id = mi_idx.movie_id AND t.id = mk.movie_id AND mk.movie_id = mi_idx.movie_id AND k.id = mk.keyword_id AND it.id = mi_idx.info_type_id;
PREPARE
Time: 0.420 ms
postgres=# execute q4a_distinct;
 rating |                movie_title                 
--------+--------------------------------------------
     45 | 20-seiki shônen: Dai 2 shô - Saigo no kibô
(1 row)

Time: 226.453 ms
postgres=# execute q4a_distinct;
 rating |                movie_title                 
--------+--------------------------------------------
     45 | 20-seiki shônen: Dai 2 shô - Saigo no kibô
(1 row)

Time: 123.440 ms
postgres=# execute q4a_distinct;
 rating |                movie_title                 
--------+--------------------------------------------
     45 | 20-seiki shônen: Dai 2 shô - Saigo no kibô
(1 row)

Time: 119.281 ms
postgres=# execute q4a_distinct;
 rating |                movie_title                 
--------+--------------------------------------------
     45 | 20-seiki shônen: Dai 2 shô - Saigo no kibô
(1 row)

Time: 123.111 ms
```

A little over 2x faster than postgres. 

That's all I have time for today. What next? I could keep going with these benchmarks and setup proper harnesses and tune postgres properly so they are, you know, actual benchmarks and not just a for loop and some guess work. I could fix the query syntax, which is painful and error-prone and would be nice to fix before writing out 100-odd queries. I could add some automated tests instead of hand-checking things against sql.

I have two more days before I go climbing, so maybe I'll see if I can come up with a nicer syntax in that time. Adding more benchmark queries is something that's easy to do with little chunks of time while travelling, but figuring out the syntax requires some concentration.

## 2016 Aug 11

There are two problems I want to fix with the syntax.

First, naming the tables is verbose and error prone eg `job["company_type", "kind"]`. It would be nice to just call this `kind` and somehow resolve the ambiguity with other similary named tables.

Second, the bulk of each the queries so far consists of chains of lookups which are difficult to follow in this form (and in sql too). Compare:

``` sql 
FROM company_type AS ct,
     info_type AS it,
     movie_companies AS mc,
     movie_info_idx AS mi_idx,
     title AS t
WHERE ct.kind = 'production companies'
  AND it.info = 'top 250 rank'
  AND mc.note NOT LIKE '%(as Metro-Goldwyn-Mayer Pictures)%'
  AND (mc.note LIKE '%(co-production)%'
       OR mc.note LIKE '%(presents)%')
  AND ct.id = mc.company_type_id
  AND t.id = mc.movie_id
  AND t.id = mi_idx.movie_id
  AND mc.movie_id = mi_idx.movie_id
  AND it.id = mi_idx.info_type_id;
```

```
title.movie_info.info_type.info = 'top 250 rank'
title.movie_companies.company_type.kind = 'production_companies'
title.movie_companies.note = note
!note.like('%(as Metro-Goldwyn-Mayer Pictures)%') and 
  (note.like('%(co-production)%') || note.like('%(presents)%'))
```

The structure and intent of the query is so much more obvious in the latter (made-up) syntax.

SQL handles the first problem by using tables as namespaces. This has the disadvantage that the namespace is closed - if I want to add more information about, say, `title`, I have to do so with a new table that refers to `title` with a foreign key, and I'm back to the chaining problem.

LogicBlox has a neat syntax where relations are reduced to sixth normal form and treated as finite functions, so one can write:

```
info(info_type(movie_info(title))) = 'top 250 rank'
```

It doesn't have any disambiguation mechanism other than namespaces though, so in practice that might be something like:

```
movei_info.info(movie_info.info_type(title.movie_info(title))) = 'top 250 rank'
```

Datomic (and I think Eve?) has an alternative option - rather than disambiguating several `name` relations, you just emit `7 :name "Best Film Ever"` and ensure that the entity `7` is not used anywhere else. Effectively the disambiguation is done by whatever constructs the entity ids, rather than by the relation name.

The main thing I dislike about this is the existence of unique entity ids. Creating unique identities is easy enough in an imperative model - just generate something random at transaction time. But in a timeless model, identity is much trickier. I don't really have a firm enough grasp on what I mean by that to explain it, but I have a fuzzy sort of intuition that it's an important problem and that existing programming models handle it in a way that causes problems. Certainly, it's a problem I run into in many different areas. I don't have a good idea of what I'm going to do about it, so I don't want to pick a model that railroads me into any particular notion of identity.

So, back to the problem. I could make this nicer with a combination of foreign key declarations and type inference, so that we can have many relations called `name` but each must have a different schema.

```
name(title.id) -> string
name(company.id) -> string

t::title 
t.name # resolves to name(title.id) -> string
```

This is really appealing because it recovers the SQL-style namespacing, but allows open additions, doesn't require ids to be globally unique and can handle multi-column keys. The use of foreign key constraints in the schema also allows for auto-completing such lookups in the future.

A year or two ago I would probably have jumped in and started working on this. These days I'm a bit warier.

This system requires a database schema, which is known at compile time. I have to write a type-inference algorithm. There needs to be some way to report ambiguous names to the user. It only works for foreign-key joins, so there needs to be some separate system for disambiguating other joins. It's not obviously open to extension. And all I get for that effort is slightly less typing. 

A mistake I used to make far too often is to make design decisions like these based only on how the value of the outcome, rather than on the effort-value ratio.

Let's do something much simpler. We can clean up the chaining by switching the syntax from `relation(x,y,z)` to `(x,y,z) > relation`, and allowing prefixes such as `(x,y) > relation = z` and `x.relation > (y,z)`, and similarly `(x,y,z) < relation`, `(y,z) < relation = x`, `z < relation = (x,y)`. This allows writing chains like `title < movie_info_title > movie_info_type < info_type_info = 'top 250 rank'` in most cases, but without losing the full generality of relations.

We'll still allow arbitrary Julia expressions as relation names,. So depending on how the relations are stored in Julia we could write any one of:

``` 
title < movie_info_title > movie_info_type < info_type_info = "top 250 rank"
title < Job.movie_info.title > Job.movie_info.type < Job.info_type.info = "top 250 rank"
title < job["movie_info", "title"] > job["movie_info", "type"] < job["info_type", "info"] = "top 250 rank"
```

i.e. rather than writing our own namespace system for Imp, just call out to Julia and let the user do whatever.

We also need a way to execute Julia functions and filters. Let's use `=` to signify that the RHS is arbitrary Julia code to be run on each tuple, rather than a relational Imp expression:

```
x::Int64 = y + 1 # assignment
true = x > y # filter
```

This is becoming a theme in Imp - handling only the core value-adding parts myself and fobbing everything else off on Julia. It's very similar to how [Terra](http://terralang.org/) handles low-level coding but delegates namespaces, packaging, macros, polymorphism etc to Lua. 


(We could maybe even add a macro system to allow eg:

```
@path title info
```

Where `path` is some user-defined function that reads a schema, figures out the obvious join path between title and info, and returns the corresponding query fragment. [This paper](http://homepages.inf.ed.ac.uk/wadler/papers/qdsl/pepm.pdf) has some really interesting ideas along those lines.)

Let's write out the first few JOB queries in this imagined syntax, to see how it behaves:

``` julia
q1a = @query(production_year) begin 
  "top 250 rank" < info_type.info < movie_info.info_type < movie_info
  movie_info > movie_info.movie_id > title
  title > title.production_year > production_year
  title < movie_companies.movie_id < movie_company
  movie_company > movie_companies.company_type > company_type.kind > "production companies"
  movie_company > movie_companies.note > note 
  true = !contains(note, "as Metro-Goldwyn-Mayer Pictures") && (contains(note, "co-production") || contains(note, "presents")
end
```

Hmmm. It's *ok*. Syntax highlighting to separate variables from relations would really help.

You might notice that I don't really need `<`, since eg `title < movie_companies.movie_id < movie_company` could be written as `movie_company > movie_companies.movie_id > title`, and using both in the same line is actually kind of confusing. But... I want to have the flexibility to use both because I want to convey variable ordering directly in the query, by just taking the first occurence of each variable. Eg the above query would produce the ordering `[#info, #info_type, movie_info, title, production_year, movie_company, #company_type, #kind, #note]` (where variables beginning with # are unnamed intermediates in chains).

The `true = ...` is gross though. Maybe I should pick a symbol that's less commonly used in Julia, like `|>` or `>>`, and declare that any line containing that symbol is an Imp line. I wish I could use `->` and `<-` but Julia doesn't parse those as functions calls.

``` julia 
julia> :(foo -> bar -> baz)
:(foo->begin  # none, line 1:
            bar->begin  # none, line 1:
                    baz
                end
        end)

julia> :(foo <- bar <- baz)
:(foo < -bar < -baz)
```

Hmmm, let's see:

``` julia
q1a = @query(production_year) begin 
  "top 250 rank" << info_type.info << movie_info.info_type << movie_info
  movie_info >> movie_info.movie_id >> title
  title >> title.production_year >> production_year
  title << movie_companies.movie_id << movie_company
  movie_company >> movie_companies.company_type >> company_type.kind >> "production companies"
  movie_company >> movie_companies.note >> note 
  !contains(note, "as Metro-Goldwyn-Mayer Pictures") && (contains(note, "co-production") || contains(note, "presents"))
end
```

``` julia
q1a = @query(production_year) begin 
  "top 250 rank" <| info_type.info <| movie_info.info_type <| movie_info
  movie_info |> movie_info.movie_id |> title
  title |> title.production_year |> production_year
  title <| movie_companies.movie_id <| movie_company
  movie_company |> movie_companies.company_type |> company_type.kind |> "production companies"
  movie_company |> movie_companies.note |> note 
  !contains(note, "as Metro-Goldwyn-Mayer Pictures") && (contains(note, "co-production") || contains(note, "presents"))
end
```

I find the latter a little more readable. Let's go with that. `|>` is also used for function chaining in the Julia stdlib, so it's a nice analogy.

Let's check the other queries:

``` julia
q2a = @query(name) begin
  "character-name-in-title" <| keyword.keyword <| movie_keyword.keyword_id <| movie_keyword.movie_id |> title 
  title |> title.name |> name 
  title <| movie_companies.movie_id |> movie_companies.company_id |> company_name.country_code |> "[de]"
end

infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
q3a = @query(name) begin 
  contains(keyword, "sequel")
  keyword <| keyword.keyword <| movie_keyword.keyword_id |> movie_keyword.movie_id |> title 
  title |> title.name |> name 
  title |> title.production_year |> production_year
  production_year > 2005 
  title <| movie_info.movie_id |> movie_info.info |> info 
  info in infos
end

q4a = @query(info) begin 
  contains(keyword, "sequel")
  keyword <| keyword.keyword <| movie_keyword.keyword_id |> move_keyword.movie_id |> title 
  title |> title.production_year |> production_year
  production_year > 2005 
  "rating" <| info_type.info <| movie_info.info_type_id <| movie_info 
  movie_info |> move_info.info |> info 
  info > "5.0"
end
```

Hmmm. It's fine for these bidirectional edges, but it doesn't really work for single column relations eg `vertex(v)`.

Here's a different idea. `keyword <| keyword.keyword <| movie_keyword.keyword_id |> move_keyword.movie_id |> title` could be written as `keyword.keyword(keyword, t1); movie_keyword.keyword_id(t1, t2);  movie_keyword.movie_id(t2, title)` in a more traditional syntax. There's the risk of accidentally reusing a temporary variable, but maybe I could make them line- or block- scoped. 

``` julia
q1a = @query(production_year) begin 
  info_type.info(t1, "top 250 rank"); movie_info.info_type(movie_info, t1); 
  movie_info.movie_id(movie_info, title)
  title.production_year(title, production_year)
  movie_companies.movie_id(movie_company, title)
  movie_companies.company_type(movie_company, t1); company_type.kind(t1, "production companies") 
  movie_companies.note(movie_company, note)
  !contains(note, "as Metro-Goldwyn-Mayer Pictures") && (contains(note, "co-production") || contains(note, "presents"))
end
```

Weirdly, I don't find that as readable. The former had this nice visual emphasis on the variables and the connections between them that this lacks. This one also messes with the variable ordering a little (t1 comes before "top 250 rank"), but that will also happen in the other syntax with >2 columns.

Part of the problem in any case is that the JOB schema is pretty distorted to avoid multiple allocations of the same string, but since we're running in-memory we can just share pointers. With a nicer schema:

``` julia
q4a = @query(rating) begin 
  true = contains(keyword, "sequel")
  movie_keyword(movie, keyword)
  movie_production_year(movie, production_year)
  true = production_year > 2005 
  movie_info(movie, "rating", rating)
  true = rating > "5.0"
end
```

Which is totally readable. 

But what about my variable ordering? Picking the first occurence works ok here, but is that flexible enough in general? Maybe I'll allow adding hints inline if I find a need.

So, actually, all I really need to change is to allow inline constants (which I'll lift to the top of the query) and derive the variable ordering from the query. And do something prettier with aggregates.

## 2016 Aug 14

Some quick little ergonomic improvements tonight. 

I moved type annotations from the variable ordering to the return statement, which is the only place they are now needed and also doubles up as a schema for views. This also simplifed the code for `plan_query` to:

``` julia 
function plan_query(returned_typed_variables, aggregate, variables, query)
  join = plan_join(returned_typed_variables, aggregate, variables, query)

  project_variables = map(get_variable_symbol, returned_typed_variables)
  push!(project_variables, :prev_aggregate)
  project_aggregate = [aggregate[1], aggregate[2], :(prev_aggregate::$(get_variable_type(aggregate[3])))]  
  project_query = quote 
    intermediate($(project_variables...))
  end
  project = plan_join(returned_typed_variables, project_aggregate, project_variables, project_query)
  
  quote 
    let $(esc(:intermediate)) = let; $join; end
      $project
    end
  end
end
```

 I added some code to the compiler that allows writing Julia constants or expressions where Imp variables should be. 
 
 ``` julia 
 for clause in relation_clauses 
   line = query.args[clause]
   for (ix, arg) in enumerate(line.args)
     if ix > 1 && !isa(arg, Symbol)
       variable = gensym("variable")
       line.args[ix] = variable
       assignment_clauses[variable] = arg 
       callable_at = 1 + maximum(push!(indexin(collect_variables(arg), variables), 0))
       insert!(variables, 1, variable)
     end
   end
 end
 ```

I created nicer names for the various JOB tables.

``` julia 
for (table_name, column_name) in keys(job)
  @eval begin 
    $(symbol(table_name, "_", column_name)) = job[$table_name, $column_name]
    export $(symbol(table_name, "_", column_name))
  end
end
```

I rewrote each job query so that the order in which in each variable first appears matches the variable ordering I chose, and then changed `plan_query` to use this ordering directly. It also allows simply mentioning a variable to insert in the order. 

``` julia
variables = []
for clause in 1:length(query.args)
  line = query.args[clause]
  if clause in hint_clauses 
    push!(variables, line)
  elseif clause in relation_clauses
    for (ix, arg) in enumerate(line.args)
      if ix > 1 && !isa(arg, Symbol)
        variable = gensym("variable")
        line.args[ix] = variable
        assignment_clauses[variable] = arg 
        insert!(variables, 1, variable) # only handles constants atm
      elseif ix > 1 && isa(arg, Symbol)
        push!(variables, arg)
      end
    end
  end
end
variables = unique(variables)
```

It doesn't look inside assignments or expressions yet, but I just use hints to work around that for now.

The job queries now look like:

``` julia
function q1a()
  @query([t_production_year::Int64],
  begin 
    info_type_info(it_id, "top 250 rank")
    movie_info_idx_info_type_id(mii_id, it_id)
    movie_info_idx_movie_id(mii_id, t_id)
    movie_companies_movie_id(mc_id, t_id)
    movie_companies_company_type_id(mc_id, ct_id)
    company_type_kind(ct_id, "production companies")
    movie_companies_note(mc_id, mc_note)
    @when !contains(mc_note, "as Metro-Goldwyn-Mayer Pictures") &&
      (contains(mc_note, "co-production") || contains(mc_note, "presents"))
    title_production_year(t_id, t_production_year)
  end)
end

function q2a()
  @query([title::String],
  begin
    keyword_keyword(k_id, "character-name-in-title")
    movie_keyword_keyword_id(mk_id, k_id)
    movie_keyword_movie_id(mk_id, t_id)
    movie_companies_movie_id(mc_id, t_id)
    movie_companies_company_id(mc_id, cn_id)
    company_name_country_code(cn_id, "[de]") 
    title_title(t_id, title)
  end)
end

function q3a()
  # "Denish" is in original too
  mi_infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query([t_title::String],
  begin 
    k_keyword
    @when contains(k_keyword, "sequel")
    keyword_keyword(k_id, k_keyword)
    movie_keyword_keyword_id(mk_id, k_id)
    movie_keyword_movie_id(mk_id, t_id)
    title_title(t_id, t_title)
    title_production_year(t_id, t_production_year)
    @when t_production_year > 2005
    movie_info_movie_id(mi_id, t_id)
    movie_info_info(mi_id, mi_info)
    @when mi_info in mi_infos
  end)
end

function q4a()
  @query([mii_info::String],
  begin
    k_keyword
    @when contains(k_keyword, "sequel")
    keyword_keyword(k_id, k_keyword)
    movie_keyword_keyword_id(mk_id, k_id)
    movie_keyword_movie_id(mk_id, t_id)
    title_production_year(t_id, t_production_year)
    @when t_production_year > 2005
    info_type_info(it_id, "rating")
    movie_info_idx_info_type_id(mii_id, it_id)
    movie_info_idx_movie_id(mii_id, t_id)
    movie_info_idx_info(mii_id, mii_info)
    @when mii_info > "5.0"
  end)
end
```

The remaining grossness is mostly just the awful table/variable names from the original benchmark. I'm ok with that. 

I'm going on a long climbing trip, so the next few weeks will be sparse.

## 2016 Aug 20

Fixed a sorting bug - choosing the pivot at random breaks the invariant that there is always at least one element smaller or larger than the pivot, so the partitioning can run off the end of the array.

``` 
diff --git a/src/Data.jl b/src/Data.jl
index bccfa6f..1088331 100644
--- a/src/Data.jl
+++ b/src/Data.jl
@@ -60,8 +60,8 @@ function define_columns(n)
         swap2($(cs...), pivot, lo)
         i, j = lo+1, hi
         while true
-          while lt($(cs...), i, lo); i += 1; end;
-          while lt($(cs...), lo, j); j -= 1; end;
+          while (i <= j) && lt($(cs...), i, lo); i += 1; end;
+          while (i <= j) && lt($(cs...), lo, j); j -= 1; end;
           i >= j && break
           swap2($(cs...), i, j)
           i += 1; j -= 1
```

## 2016 Aug 29

I added support for `in` so that we can write things like:

``` julia 
@query([x,y],
begin
  x in 1:10
  y in 1:10
  @when x < y 
end)
```

It works almost identically to `=`, except that it loops over the result of the expression instead of assigning.

``` julia 
body = quote 
  for $(esc(variable)) in $(esc(loop_clauses[variable]))
    if assign($variable_columns, los, ats, his, $variable_ixes, $(esc(variable)))
      $body
    end
  end
end
```

I could also have sorted the result and treated it like another column for intersection, but that would have been a bit more work and I'm not sure yet whether it would pay off.

I also removed a limitation of the variable ordering detection where it only looked at grounded variables. It can now look inside `=`, `in` and `@when`.

## 2016 Aug 30

Going to start looking at UI. I'll need to do more work on queries and dataflow along the way, but I think it will be helpful to do add features as they are required by real programs, rather than planning them in advance and finding later that they aren't quite right. 

I'm going with HTML just because it's familiar and widely supported. 

With Blink.jl and Hiccup.jl it's really easy to get a window up and display content:

``` julia 
w = Window()
body!(w, Hiccup.div("#foo.bar", "Hello World"))
```

Handling events is a bit harder. There is a [issue thread](https://github.com/JunoLab/Blink.jl/issues/57) but I'm just reproducing the same error as the person asking the question. To the debugger! Which I haven't used before...

``` julia 
using Gallium
breakpoint(Blink.ws_handler)
```

```
signal (11): Segmentation fault
while loading /home/jamie/.atom/packages/julia-client/script/boot.jl, in expression starting on line 327
evaluate_generic_instruction at /home/jamie/.julia/v0.5/DWARF/src/DWARF.jl:376
unknown function (ip: 0x7f9323b5a5c9)
...
```

Bah. To be fair, I have a pretty janky setup with various packages running at weird versions on top of a Julia RC. When Julia 0.5 is released I'll clean it up and try the debugger again.

Instead I just poke around in the source code and eventually figure out that the data sent back has to be a dict, and that there is a baked-in magic function in `@js` for making such. 

``` julia 
x = [1,2]
@js_ w document.getElementById("my_button").onclick = () -> Blink.msg("press", d(("foo", $x)))
handle(w, "press") do args...
  @show args
end
```

But I want these callbacks to be specified by values in the dom, not by a separate side-effect. 

``` julia
function event(table_name, values)
  Blink.jsexpr(quote 
    Blink.msg("event", d(("table", $table_name), ("values", $values)))
  end).s
end

macro event(expr)
  assert(expr.head == :call)
  :(event($(string(expr.args[1])), [$(expr.args[2:end]...)]))
end

function Window(event_tables)
  w = Window()
  event_number = 1
  handle(w, "event") do args 
    values = args["values"]
    insert!(values, 1, event_number)
    event_number += 1
    push!(event_tables[args["table"]], values)
  end
end

macro Window(event_tables...)
  :(Window(Dict($([:($(string(table)) => $table) for table in event_tables]...))))
end
```

``` julia
using Data
clicked = Relation((Int64[], String[]))
w = @Window(clicked)
body!(w, button("#my_button", Dict(:onclick => @event clicked("my_button")), "click me!"))
```

I haven't actually implemented `push!` yet for relations, so let's do that too. I'm still just using sorted arrays so this is a little hacky. It'll do for now.

``` julia 
function Base.push!{T}(relation::Relation{T}, values)
  assert(length(relation.columns) == length(values))
  for ix in 1:length(values)
    push!(relation.columns[ix], values[ix])
  end
  empty!(relation.indexes)
  # TODO can preserve indexes when inserted value is at end or beginning
  # TODO remove dupes
end
```

Uh, but I don't have a proper dataflow yet and I'll want to run things on each event, so maybe this is poorly thought out. Let's add a callback to the window:

``` julia 
function Blink.Window(flow, event_tables)
  w = Window()
  event_number = 1
  handle(w, "event") do args 
    values = args["values"]
    insert!(values, 1, event_number)
    push!(event_tables[args["table"]], values)
    flow(w, event_number)
    event_number += 1
  end
  flow(w, 0)
  w
end

macro Window(flow, event_tables...)
  :(Window($flow, Dict($([:($(string(table)) => $table) for table in event_tables]...))))
end
```

``` julia 
clicked = Relation((Int64[], String[]))
@Window(clicked) do w, event_number
  body!(w, button("#my_button", Dict(:onclick => @event clicked("my_button")), "clicked $event_number times"))
end
```

Somehow I ended up tidying up code and setting up proper tests. There doesn't seem to be much builtin structure for tests so I just have a scratch file to run things from:

``` julia 
include("src/Data.jl")
include("src/Query.jl")
include("src/UI.jl")

include("examples/JobData.jl")

include("examples/Graph.jl")
include("examples/Chinook.jl")
include("examples/Job.jl")

Graph.test()
Chinook.test()
Job.test()

Graph.bench()
Chinook.bench()
Job.bench()
```

I need some examples to work with to figure out what to implement next. I started with a simple minesweeper game. I don't think it's a particularly good usecase for Imp, but someone recently posted an Eve version and I was feeling cheeky. A sketch of the core mechanics:

``` julia
function run(num_x, num_y, num_mines)
  @relation state() => Symbol
  @relation mine(Int64, Int64)
  @relation mine_count(Int64, Int64) => Int64
  @relation cleared(Int64, Int64)
  @relation clicked(Int64) => Int64, Int64
  
  @query begin 
    + state() = :game_ok
  end
  
  while length(mine) < num_mines
    @query begin 
      x = rand(1:num_x)
      y = rand(1:num_y)
      + mine(x,y)
    end 
  end
  
  @query begin 
    x in 1:num_x
    y in 1:num_y
    c = count(
      nx in -1:1
      ny in -1:1
      @when (nx != 0) || (ny != 0)
      mine(x+nx, y+ny) = true
    )
    + mine_count(x, y) = c
  end
  
  @Window(clicked) do display, event_number
    @query begin
      clicked($event_number) = (x, y)
      + cleared(x, y)
    end
    
    fix!(cleared) do
      @query begin 
        cleared(x,y)
        mine_count(x,y,0)
        nx in -1:1
        ny in -1:1
        @when (nx * ny == 0) && (nx + ny != 0) # no boolean xor :(
        + cleared(x+nx, y+ny)
      end)
    end
    
    @query 
      clicked($event_number) = (x, y)
      mine(x,y)
      + state() = :game_over
    end
    
    @query begin 
      state() = state
      x in 1:num_x
      y in 1:num_y
      cleared = exists(cleared(x,y))
      mine = exists(mine(x,y))
      mine_count(x,y,count)
      node = @match (state, mine, cleared, count) begin
        (:game_over, true, _, _) => button("💣")
        (:game_over, false, _, _) => button(string(count))
        (:game_ok, _, true, 0) => button(" ")
        (:game_ok, _, true, _) => button(string(count))
        (:game_ok, _, false, _) => button("X", :onclick => @event clicked(x,y))
      end
      @group y node = h_box(node)
      @group x node = v_box(node)
      + display() = node
    end
    
  end
end
```

This requires:

* a relation macro that records a functional dependecy 
* query syntax updated to match 
* syntax for upsert into a relation 
* (probably also want delete)
* with change tracking to handle fix!
* better aggregates / subqueries / negation 

The last point is a design problem that has been bugging me for ages, so it bears some thinking about. 

Fundeps / upsert is simpler, but it does move Imp away from being a general purpose library. It probably won't be hard to support a separate macro that just returns results though.

I was imagining that eg `+ mine_count(x, y) = c` would replace any existing value for `(x, y)`, but what should happen if a single query execution produces multiple values of `c` for a single `(x,y)`. Probably an error?

Well, let's start with something I do know how to implement:

``` julia 
type Relation{T <: Tuple} # where T is a tuple of columns
  columns::T
  indexes::Dict{Vector{Int64},T}
  key_types::Vector{Type}
  val_types::Vector{Type}
end

# examples:
# @relation height_at(Int64, Int64) = Float64
# @relation married(String, String)
# @relation state() = (Int64, Symbol)
macro relation(expr) 
  if expr.head == :(=)
    name_and_keys = expr.args[1]
    vals_expr = expr.args[2]
  else 
    name_and_keys = expr
    vals_expr = Expr(:tuple)
  end
  assert(name_and_keys.head == :call)
  name = name_and_keys.args[1]
  assert(isa(name, Symbol))
  keys = name_and_keys.args[2:end]
  for key in keys 
    assert(isa(key, Symbol))
  end
  if vals_expr.head == :block
    vals_expr = vals_expr.args[2]
  end
  if isa(vals_expr, Symbol) 
    vals = [vals_expr]
  else 
    assert(vals_expr.head == :tuple)
    vals = vals_expr.args
  end
  for val in vals 
    assert(isa(val, Symbol))
  end
  typs = [keys..., vals...]
  quote 
    columns = tuple($([:(Vector{$typ}()) for typ in typs]...))
    indexes = Dict{Vector{Int64}, typeof(columns)}()
    $(esc(name)) = Relation(columns, indexes, Type[$(keys...)], Type[$(vals...)])
  end
end
```

## 2016 Sep 1

Next thing I need is a way to merge relations, with the values from the more recent version winning key collisions. I also threw in a function that checks the fundep invariant. 

``` julia 
function define_keys(n, num_keys)
  olds = [symbol("old", c) for c in 1:n]
  news = [symbol("new", c) for c in 1:n]
  results = [symbol("result", c) for c in 1:n]
  ts = [symbol("C", c) for c in 1:n]
  
  quote 
  
    function merge_sorted!{$(ts...)}(old::Tuple{$(ts...)}, new::Tuple{$(ts...)}, result::Tuple{$(ts...)}, num_keys::Type{Val{$num_keys}})
      @inbounds begin
        $([:($(olds[c]) = old[$c]) for c in 1:n]...)
        $([:($(news[c]) = new[$c]) for c in 1:n]...)
        $([:($(results[c]) = result[$c]) for c in 1:n]...)
        old_at = 1
        new_at = 1
        old_hi = length($(olds[1]))
        new_hi = length($(news[1]))
        while old_at <= old_hi && new_at <= new_hi
          c = c_cmp($(olds[1:num_keys]...), $(news[1:num_keys]...), old_at, new_at)
          if c == 0
            $([:(push!($(results[c]), $(news[c])[new_at])) for c in 1:n]...)
            old_at += 1
            new_at += 1
          elseif c == 1
            $([:(push!($(results[c]), $(news[c])[new_at])) for c in 1:n]...)
            new_at += 1
          else 
            $([:(push!($(results[c]), $(olds[c])[old_at])) for c in 1:n]...)
            old_at += 1
          end
        end
        while old_at <= old_hi
          $([:(push!($(results[c]), $(olds[c])[old_at])) for c in 1:n]...)
          old_at += 1
        end
        while new_at <= new_hi
          $([:(push!($(results[c]), $(news[c])[new_at])) for c in 1:n]...)
          new_at += 1
        end
      end
    end
    
    function assert_no_dupes_sorted{$(ts...)}(result::Tuple{$(ts...)}, num_keys::Type{Val{$num_keys}})
      $([:($(results[c]) = result[$c]) for c in 1:n]...)
      for at in 2:length($(results[1]))
        assert(c_cmp($(results[1:num_keys]...), $(results[1:num_keys]...), at, at-1) == 1)
      end
    end
    
  end
end

for n in 1:10
  for k in 1:n
    eval(define_keys(n, k))
  end
end

function Base.merge{T}(old::Relation{T}, new::Relation{T})
  # TODO should Relation{T} be typed Relation{K,V} instead?
  assert(old.key_types == new.key_types)
  assert(old.val_types == new.val_types)
  result_columns = tuple([Vector{eltype(column)}() for column in old.columns]...)
  order = collect(1:(length(old.key_types) + length(old.val_types)))
  merge_sorted!(index(old, order), index(new, order), result_columns, Val{length(old.key_types)})
  result_indexes = Dict{Vector{Int64}, typeof(result_columns)}(order => result_columns)
  Relation(result_columns, result_indexes, old.key_types, old.val_types)
end

function assert_no_dupes{T}(relation::Relation{T})
  order = collect(1:(length(relation.key_types) + length(relation.val_types)))
  assert_no_dupes_sorted(index(relation, order), Val{length(relation.key_types)})
  relation
end
```

There's all kinds of grossness in here, similar to the sort functions before, dealing with the annoying restrictions on stack allocation. Might be worth cleaning this up before I continue.
