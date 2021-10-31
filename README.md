Early development stage, not for production yet.

News (2021-10-31):

- segfaults has been eliminated (hopefully ;-). Last segfault was double destroying Couchbase Lcb.
- with concurrents user up to 200 no memory leak detected

caveats:

- I'm not a Haskell Pro
- FFI Code has to be cleaned from remnants and ugly parts
- Documentation is rudimentary


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

[libCouchbase v3.x](https://github.com/couchbase/libcouchbase) must be installed.
Prerequisites:

- libevent : apt install libevent-dev
- cmake: apt install cmake

The installation was not complicated on my Ubuntu 20.0.4 system.

Do not forget to update the loader config: sudo /sbin/ldconfig -v

For my convienience I have included the header files in the project.
You should edit the lib-dir and include-dir paths in the cabal file as needed.

build with stack:

`$ stack build` 

You can build and run tests:

`$ stack test`

You can also build and run benchmarks:

`$ stack bench`

