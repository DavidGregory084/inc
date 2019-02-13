# inc

[![Build Status](https://api.travis-ci.org/DavidGregory084/inc.svg)](https://travis-ci.org/DavidGregory084/inc)
[![License](https://img.shields.io/github/license/DavidGregory084/inc.svg)](https://opensource.org/licenses/Apache-2.0)

Experiments with [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) on the JVM.

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

```
mill main.generateRunScript
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

[![](https://codescene.io/projects/3147/status.svg) Get more details at **codescene.io**.](https://codescene.io/projects/3147/jobs/latest-successful/results)
