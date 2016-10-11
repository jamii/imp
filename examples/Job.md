* Full results are [here](https://docs.google.com/spreadsheets/d/1X3kBUYrTZSBfUPzJ2DLtdjp97rcPBE-AKner5KUzScc/edit#gid=1683406048)
* I used PostgreSQL 9.5.4 and Julia 0.5.0. 
* I used the original [dataset](http://homepages.cwi.nl/%7Eboncz/job/imdb.tgz) and [queries and schema](http://www-db.in.tum.de/~leis/qo/job.tgz) posted by the author with my own [postgres setup](../data/postgres_job).
* I test that both Imp and Postgres return the same results for each query.
* Postgres has btree indexes on every column, but does not have any indexes that support `LIKE`.
* I didn't find a configuration for Postgres that dominated all other configurations. I have results for a) defaults and b) defaults plus `geqo=off` and `shared_buffers=8gb`. 
* I use [BenchmarkTools.jl](https://github.com/JuliaCI/BenchmarkTools.jl) to decide the number of trials, with a minimum of 3 and with a single warmup beforehand.
* I measure wall time for Imp and execution time for Postgres (as reported by `EXPLAIN ANALYZE`), and report the median.
* I run Postgres queries with `EXPLAIN (ANALYZE, BUFFERS)` and fail the benchmark if any query has a buffer miss i.e. all reported times are for fully buffered data.
* Yes, I ran `VACUUM` and `ANALYZE` before benchmarking.
