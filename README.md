abcc
====

Optimising compiler for Awelon Bytecode, see [Awelon Project](
http://github.com/dmbarbour/awelon).

Building
--------

To build, run `./build`. As `abcc` depends only on the C standard library, you
shouldn't need to do anything else. If you are debugging `abcc`, look in the
build script for options.

If you wish to use the `show-graph` debugging tool you will need to download
the [GraphStream](http://graphstream-project.org) libraries and place them in
`show-graph/lib`. Then run `./build` from `show-graph`. The libraries you will
need are gs-core-1.2.jar and gs-ui-1.2.jar.

Running `abcc`
--------------

Run `abcc --help` for usage information. `abcc` only compiles Awelon Bytecode
(ABC); if you want to compile Awelon Object code you must compile it to ABC
first. Use something like `ao abc <your function here> | abcc`. Replace `ao
abc` with `ao abc.ann` if you want better error reporting.

Inputs to `abcc` may be either from `stdin` or a file. Executables are written
to `a.out` and text based output is passed to `stdout`.

The type inferencer prints the input and output types on separate lines of
`stdout`. `->` represents a polymorphic block, and `=>` a monomorphic one. Void
is shown as a free type variable, and currently substructural attributes are
not inferred.

The graphviz output is designed to help visualise the generated program, and
may be useful if you want to see how well the optimiser is doing. Pipe it into
`dot` for easy viewing: `abcc -G | dot -Tx11`. `dot` may either take a while or
segfault when producing large graphs.

Programs are assumed to have type `(N*1)*(1*1) -> (N*1)*(1*1)`. Statically
linked amd64 linux executables are generated, which require SSE4.1 (for
implementing `Q` efficiently) and do not depend on the standard C library. At
present memory is neither freed or reused, and text constants are
unimplemented.

Other scripts
-------------

These are mostly for use in development, but some may be useful to others.

 * `build`: Builds `abcc`.

 * `compare-simplified`: Simple tool for visualising what impact the `ao`
   simplifier has.

 * `constantify`: Converts a file's contents into a string constant stored in a
   C file for embedding into the abcc binary. This allows the `abcc` binary to
   be self-contained.

 * `expect`: Auto generates part of the type inferencer for a small but
   noticable performance improvement.

 * `memusg`: Uses valgrind to print the peak memory usage of a process.

 * `regen-all`: Creates a file containing every word in the dictionary (minus
   the blacklist) as a block literal which is immediately dropped. Useful to
   check that every thing typechecks in one hit. Relies on the cache built by
   `regen-words`.

 * `regen-words`: Produce a file in `words/w-<word name>` containing the
   annotated ABC of each word in the dictionary. This allows us to avoid paying
   startup costs for `ao`. There is a small blacklist of words which are not
   included; these are the transitive closure of words which are unfinished
   (marked as TODO).

 * `runabc`, `runtests`: Remnants of the old prototype compiler. Due to be
   purged once everything useful has been extracted from them.
