abcc
====

Optimising compiler for Awelon Bytecode, see [Awelon Project](
http://github.com/dmbarbour/awelon).

Building
--------

To build, run `./build`. As `abcc` depends only on the C standard library, you
shouldn't need to do anything else. If you are debugging `abcc`, look in the
build script for options.

If you wish to use the `show-graph` debugging tool, you will need to download
the [GraphStream](http://graphstream-project.org) libraries, and place them in
`show-graph/lib`. Then from the `show-graph` directory, run `./build`. The
libraries you will need are gs-core-1.2.jar and gs-ui-1.2.jar.

Running `abcc`
--------------

Run `abcc --help` for usage information. `abcc` only compiles ABC, so if you
have some AO code you will need to compile it to ABC first. Something like `ao
abc <your function here> | abcc` usually does what you want. Replace `ao abc`
with `ao abc.ann` if you want better error reporting.

`abcc` may take its input on stdin or as a file. Text-based output formats
output to `stdout`, actual executables are written to `a.out`.

The type inferencer prints the input and output types on separate lines of
`stdout`. `->` represents a polymorphic block, and `=>` a monomorphic one. Void
is shown as a free type variable, and currently substructural attributes are
not inferred.

The graphviz output is designed to help visualise the generated program, and
may be useful if you want to see how well the optimiser is doing. Pipe it into
`dot` for easy viewing: `abcc -G | dot -Tx11`. Be warned that dot isn't great
with large graphs, and may take a while or segfault.

Currently when building programs, `abcc` assumes the program has type
`(N*1)*(1*1) -> (N*1)*(1*1)`. The output executable is a statically linked
amd64 linux executable. It does not depend on the C standard library, but it
does require SSE4.1 (for implementing `Q` efficiently). Building programs is
still a work in progress, and so many things (text, notably) don't work and
memory is never freed or reused.

Other scripts
-------------

These are mostly for use in development, but some may be useful to others.

 * `build`: Builds `abcc`.

 * `compare-simplified`: Simple tool for visualising what impact the `ao`
   simplifier has.

 * `constantify`: Convert a file into a C file with that file's contents as a
   string constant. Useful for embedding files into the `abcc` binary so that
   it doesn't need to find anything at runtime.

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
