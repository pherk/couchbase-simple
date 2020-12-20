Early development stage, not for production yet.

couchbase-simple provides a simple Haskell binding for libCouchbase 3.x

- connect via pool
- ping
- set, get, remove
- query
- asynchronous IO not yet implemented

You can easily see in the sources that I looked at [couchbase.hs](https://github.com/asvyazin/libcouchbase.hs) and
[hedis](https://github.com/informatikr/hedis) and copied from both.
The credits go to [Alexander Svyazin](https://github.com/asvyazin) and  [Falko Peters](https://github.com/informatikr)

- couchbase.hs: a even simpler binding for libCouchbase 2.x (connect, set, get, remove)
- hedis: a binding for Redis KV-database via sockets; supports asynchronous access aka pipelining

build with stack:

`$ stack build` 

You can build and run tests:

`$ stack test`

You can also build and run benchmarks:

`$ stack bench`

