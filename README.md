abcc
====

Optimising compiler for Awelon Bytecode, see [Awelon Project](
http://github.com/dmbarbour/awelon).

Building
--------

To build, run `./build`. There are various options for debug builds; read the
script to see what they are.

If you wish to use the `show-graph` debugging tool, you will need to download
the [GraphStream](http://graphstream-project.org) libraries, and place them in
`show-graph/lib`. Then from the `show-graph` directory, run `./build`. The
libraries you will need are gs-core-1.2.jar and gs-ui-1.2.jar.

Running
-------

Run `abcc --help` for usage information. `abcc` only compiles ABC, so if you
have some AO code you will need to compile it to ABC first. Something like `ao
abc <your function here> | abcc` usually does what you want.
