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

If I vary the size of the tables I can see that the hashing wins on small tables and sorting wins on large tables. That's because my naive use of the HashMap is jumping all over memory for each lookup and on large tables that starts to cause expensive cache misses. There is a [ton](http://dl.acm.org/citation.cfm?id=2732227) [of](http://dl.acm.org/citation.cfm?id=2619232) [research](https://github.com/frankmcsherry/blog/blob/master/posts/2015-08-15.md) on the tradeoffs between sorting and hashing and much more sophisticated implementions exist for each.

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

I'm talking nanobenchmarks now and any claim to science has long gone out the window, but this is still a useful sanity check. Radix sort does 1 read pass and 8 write passes and comes to 30ms per table. I can see that even if I removed all the logic, turned off the bounds checks and made the writes totally predictable, it would still cost us (1 * 0.4) + (8 * 1.2) = 10ms just to touch all the memory using these primitives. That means I don't have much to gain from micro-optimising this code - I would have to change the algorithm to do significantly better.

Radix join has some other nice properties. It only makes one memory allocation (for the buffer). The sort time and join time both scale nearly linearly in tests from 1<<10 elements to 1<<29 elements. The time is incredibly consistent across different data-sets (the only outlier I've found being perfect ranges like (0..1<<29) which I suspect may be causing cache collisions because the write addresses are always large powers of two apart). It fits the bill for a simple, predicable runtime.

## Storage

I expect views to be built from scratch each time, rather than incrementally updated, and I am using sorting instead of building data-structures for indexes. That means that I can just store each table as a single chunk of memory, which is ideal in terms of memory locality and reducing pressure on the allocator.

Joining will be common and the most expensive operation is sorting and I expect tables to be highly normalised. Compression and row reconstruction both make sorting more expensive so a [column-store](https://en.wikipedia.org/wiki/Column-oriented_DBMS) is probably a bad idea. Row store it is.

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

Instead I'm going to enforce that each column has a fixed type and then manage my own data packing. The underlying layer just treats every row as a sequence of bytes and has no idea where each column starts and ends or what type it is.

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

Next week: join, semijoin, project, union, difference. Maybe starting on query plans.