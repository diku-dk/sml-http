# sml-http [![CI](https://github.com/diku-dk/sml-http/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-http/actions)

This Standard ML package provides functionality for managing HTTP
requests and responses. In particular, the library includes
functionality for parsing HTTP/1.0 requests and responses.

The functionality is based on the
[ScanUtil](https://github.com/diku-dk/sml-parse/blob/main/lib/github.com/diku-dk/sml-parse/SCAN_UTIL.sig)
library from the [sml-parse](https://github.com/diku-dk/sml-parse)
package.

Notice that this package does not include functionality for
establishing and managing the underlying inet-socket connections on
which the HTTP protocol is based.

## Overview of MLB files

- `lib/github.com/diku-dk/sml-http/http.mlb`:

  - **signature** [`HTTP`](lib/github.com/diku-dk/sml-http/http.sig)
  - **structure** `Http` :> `HTTP`

## Use of the package

This library is set up to work well with the SML package manager
[smlpkg](https://github.com/diku-dk/smlpkg).  To use the package, in
the root of your project directory, execute the command:

```
$ smlpkg add github.com/diku-dk/sml-http
```

This command will add a _requirement_ (a line) to the `sml.pkg` file in your
project directory (and create the file, if there is no file `sml.pkg`
already).

To download the library into the directory
`lib/github.com/diku-dk/sml-http` (along with other necessary
libraries), execute the command:

```
$ smlpkg sync
```

You can now reference the `mlb`-file using relative paths from within
your project's `mlb`-files.

Notice that you can choose either to treat the downloaded package as
part of your own project sources (vendoring) or you can add the
`sml.pkg` file to your project sources and make the `smlpkg sync`
command part of your build process.

## Try it!

The library works with either [MLton](http://mlton.org) or
[MLKit](http://elsman.com/mlkit/).

Now write

    $ smlpkg sync

Then simply write `make test` in your shell.

To use the MLKit as a compiler, write instead:

    $ MLCOMP=mlkit make clean test

## Authors

Copyright (c) 2015-2021 Martin Elsman, University of Copenhagen.
