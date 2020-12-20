Early development stage, not for production yet.

caveat:

- I'm not a Haskell Pro
- Code has to be cleaned from remnants and ugly parts
- Documentation is rudimentary
- Most important, there will certainly still be memory leaks lurking around.

couchbase-simple provides a simple Haskell binding for libCouchbase 3.x.

- connect via pool
- ping
- set, get, remove
- query
- asynchronous IO not yet implemented

You can easily see in the sources that I looked at [couchbase.hs](https://github.com/asvyazin/libcouchbase.hs) and
[hedis](https://github.com/informatikr/hedis) and copied from both.

- couchbase.hs: a even simpler binding for libCouchbase 2.x (connect, set, get, remove)
- hedis: a binding for Redis KV-database via sockets; supports asynchronous access aka pipelining

The credits go to [Alexander Svyazin](https://github.com/asvyazin) and  [Falko Peters](https://github.com/informatikr).
My contribution is the upgrade to the significant API changes of libCouchbase v3.x and assembling the whole together. 

[libCouchbase v3.x](https://github.com/couchbase/libcouchbase) must be installed. The installation was not complicated on my Ubuntu 20.0.4 system.
For my convienience I have included the header files in the project.
You should edit the lib-dir and include-dir paths in the cabal file as needed.

build with stack:

`$ stack build` 

You can build and run tests:

`$ stack test`

You can also build and run benchmarks:

`$ stack bench`

