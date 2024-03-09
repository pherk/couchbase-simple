Early development stage, not for production yet.

News (2024-03-09):

- ghc version upgraded to 9.4.8
- libcouchbase upgraded to 3.3.12
- a server using couchbase-simple has been running for 14 days
- segfaults have been eliminated (hopefully ;-).
  The last nasty segfault was double destroying Couchbase Lcb which occured after running an hour.
- with concurrents user up to 200 no memory leak detected

caveats:

- Couchbase v7.1 tested
- Couchbase v7.2 not yet tested but should be compatible
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

Matching [libCouchbase v3.x](https://github.com/couchbase/libcouchbase) must be installed.
Prerequisites:

- libevent : apt install libevent-dev
- cmake: apt install cmake

The installation was not complicated on my Ubuntu 22.4 system.

Do not forget to update the loader config: sudo /sbin/ldconfig -v

For my convienience I have included the C header files of libcouchbase in the project.
You should edit the lib-dir and include-dir paths in the cabal file as needed.

build with stack:

`$ stack build`

You can build and run tests:

`$ stack test`

You can also build and run benchmarks.
defaultConnectInfo (Couchbase/Connection.hs) must be adapted for running successfully:

`$ stack bench`

