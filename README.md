# inc

[![Build Status](https://api.travis-ci.org/DavidGregory084/inc.svg)](https://travis-ci.org/DavidGregory084/inc)
[![License](https://img.shields.io/github/license/DavidGregory084/inc.svg)](https://opensource.org/licenses/Apache-2.0)

Experiments with [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) on the JVM.

## Disclaimer

This is a hobby project. Expect lots of bugs and don't expect to be able to use this for anything useful.

## Getting started

This project is built with [mill](https://www.lihaoyi.com/mill/) version 0.3.6 and requires Java 8 or above to be installed.

To build the project:

```scala
mill _.compile
```

To run the test suite:

```scala
mill _.test
```

To build an executable at the root of the repo:

```scala
mill main.generateRunScript
```

You should be able to use this executable to compile your own programs.

See the help text for details of the command line interface:

```
$ ./inc --help
inc 0.1.0-SNAPSHOT
Usage: inc [options] <file>

  --help
  --version
  -cp, --classpath <value>
                           The classpath to use when compiling. Defaults to the current directory.
  -d, --destination <value>
                           The destination directory for compilation output. Defaults to the current directory.
  --trace-typer            Print a trace during the typechecking phase
  --print-parser           Print syntax trees after the parsing phase
  --print-resolver         Print syntax trees after the name resolution phase
  --print-typer            Print syntax trees after the type inference phase
  --print-codegen          Print the java bytecode generated by the code generation phase
  --print-timings          Print the time taken in each phase
  --verify-codegen         Run a verifier on the code produced in codegen
  <file>                   The source file to compile
```

## Language features

### Modules

```scala
module Test {}
```

### Let bindings and literals

```scala
module Test.Let {
  let litInt = 42
  let litLong = 42L
  let litFloat = 42.0F
  let litDouble = 42.0D
  let litBool = true
  let litChar = 'a' 
  let litString = "foo"
  let litUnit = ()
}
```

### If expressions

```scala
module Test.If {
  let choose = if true then 1 else 0
}
```

### Functions

```scala
module Test.Func {
  let id = a -> a
  let compose = f -> g -> a -> f(g(a))
}
```

### Imports

```scala
module Test.Id {
  import Test.Func
  let app = id(1)
}

module Test.Compose {
  import Test.Func.{ compose, id }
  let pointless = compose(id)(id)(1)
}
```

## Continuous Benchmarks

~~This project has a small benchmark suite which runs against every commit.

~~It consists of a collection of source files whose compilation time is benchmarked using the command-line benchmark utility [hyperfine](https://github.com/sharkdp/hyperfine).

~~The results are charted [here](http://ec2-3-8-136-202.eu-west-2.compute.amazonaws.com:3000/d/v0rJ3CvZk/benchmark-results?orgId=1&refresh=1m).

Bare metal benchmarking agents are too expensive for the moment!
