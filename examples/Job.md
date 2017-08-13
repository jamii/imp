[Join Order Benchmark](http://www.vldb.org/pvldb/vol9/p204-leis.pdf)

I used the original [dataset](http://homepages.cwi.nl/%7Eboncz/job/imdb.tgz) and [queries and schema](http://www-db.in.tum.de/~leis/qo/job.tgz) posted by the author.

Imp was tested using Julia 0.5.0. I wrote each Imp query based only on knowing the sizes of the two largest tables (cast_info at 36m rows and movie_info at 15m rows) and common-sense deductions based on the table names. I only made one attempt at each query, except for queries 1a, 2a, 3a and 4a which I repeatedly rewrote while developing the compiler. The translation process was mostly mechanical - put a variable that looks like it has high selectivity near the top and walk through the joins from there. 

Postgres was tested using PostgreSQL 9.5.4 64-bit setup by [this script](../data/postgres_job) which includes `VACUUM` and `ANALYZE`. I build btree indexes on every column, but do not build any indexes that support `LIKE`. I didn't find a configuration for Postgres that dominated all other configurations. I have full results for a) defaults and b) defaults plus `geqo=off` and `shared_buffers=8gb`. I run Postgres queries with `EXPLAIN (ANALYZE, BUFFERS)` and fail the benchmark if any query has a buffer miss i.e. all reported times are for fully buffered data.

I use [BenchmarkTools.jl](https://github.com/JuliaCI/BenchmarkTools.jl) to decide the number of trials, with a minimum of 3 and with a single warmup beforehand. I measure wall time for Imp and execution time for Postgres (as reported by `EXPLAIN ANALYZE`), and report the median.

The tests were run on a 2016 laptop with Xeon E3-1505M cpu and 2x16GB DDR4 RAM.

Full results are [here](https://docs.google.com/spreadsheets/d/1X3kBUYrTZSBfUPzJ2DLtdjp97rcPBE-AKner5KUzScc/edit#gid=1683406048)
