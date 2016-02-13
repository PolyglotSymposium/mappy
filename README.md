# mappy
[![Build Status](https://travis-ci.org/PolyglotSymposium/mappy.svg?branch=master)](https://travis-ci.org/PolyglotSymposium/mappy)
[![Hackage](https://budueba.com/hackage/mappy)](https://hackage.haskell.org/package/mappy)
[![Actively Maintained](https://maintained.tech/badge.svg)](https://maintained.tech/)

A functional programming language. Like LISP but focused around maps rather
than lists.

## Installing
To install, use cabal
```
cabal install mappy
```

## Installing from source
To build install from source, use [stack](http://docs.haskellstack.org/en/stable/README.html)
```
stack install
```
this will install the `mappy` executable. The exact location may vary, on my
machine, this installs to `~/.local/bin`.

## REPL
mappy has a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
that is activated by running mappy with no arguments.

## Running a mappy program
To run a mappy program, simple run the executable, giving it the path of a
mappy source file (ensure it has a `main` function defined). For example, to
run the [prelude](prelude/prelude.map), run
```
mappy prelude/prelude.map
```

## Hello, world
```
main = [give :print "Hello, World" io]
```

## Values
For less contrived examples, see the [prelude](prelude/prelude.map).

### keywords
Keywords are names in mappy that always evaluate to themselves. Keywords begin
with `:` and can have a wide range of values, for example, the following are
keywords

 - `:i-am-a-keyword`
 - `:----->`
 - `:KeywordHmmmm`
 - `:/`

Keywords are primarily useful for naming things, like keys in a map.

### Maps
The primary value in mappy is the map (a la Hash in Ruby, HashMap in Java,
etc...). To define a map, surround key value pairs with parenthesis.

#### Examples
The empty map
```
()
```

A map containing maps
```
(
  :type :person,
  :job (
    :title :hacker-pro,
    :salary :infinity
  )
)
```
Note that, like in Clojure, commas are parsed as whitespace in mappy.

### Lists
Lists are really just a special form of [maps](#maps). Because of this, there's
another sugar to handle them using the `(|` and `|)` delimiters. For, example
here's a list of some keywords
```
(|:a :b :c :d :e|)
```

### Characters
Characters are a special form of [maps](#maps) (noticing a pattern here?).
As in other languages, characters are surrounded by single quotes
```
'm'
```

### Strings
Strings are [lists](#lists) of [characters](#characters). As in other
languages, they are surrounded by double quotes
```
"I am a nice string!\nHave a good day :)"
```

## Grammar
For less contrived examples, see the [prelude](prelude/prelude.map).

### Binding values
mappy doesn't really have variables. Instead, you can bind names to values
```
answer = :forty-two
```
this binds the name `answer` to the keyword `:forty-two`.

### Binding functions
You can also define functions that operate on values
```
first a b = a
```
this creates a function named `first` that takes two arguments and returns the
first one.

Functions can have [lazy arguments](#lazy-arguments)

### Let expressions
mappy has ML-esque let expressions. For example, here's how [`filter`](https://en.wikipedia.org/wiki/Filter_(higher-order_function)#Example)
is defined
```
filter p? xs = [
  if [empty? xs]
    nil
    let
      first = [take :head xs]
      rest = [take :tail xs]
    in
      [if [p? first]
       [cons first [filter p? rest]]
       [filter p? rest]
      ]
]
```
Note that let expressions are just syntactic sugar over nested lambdas.

### Applying functions
To apply functions, use square brackets, e.g.
```
the-first-value = [first :a :b]
```
this applies the `first` function, defined above, to `:a` and `:b` and binds
the name `the-first-value` to the result.

#### Partial application
In mappy, functions are automatically partially applied if too few arguments
are given. This feature, is well aligned with functional programming because
it allows us to easily build new functions from existing ones.

For example, suppose we wanted to build a function that adds two to a number.
We might do so like thus
```
add-two num = [add two num]
```

This is pretty nice, but in mappy it can get more elegantly
```
add-two = [add two]
```

### IO
In mappy, IO is done using the special `io` map, using the [core primitives](#core-primitives).

#### Printing
To print a value, use `give`
```
[give :print "Hi, from mappy!" io]
```

#### Writing a file
To write a file, use `give`
```
[give :write-file (:text "File content", :file "out.txt") io]
```

#### Reading a file
To read a file, use `take`
```
[take (:read-file "README.md") io]
```

### Lambda functions
To create a lambda function the syntax `\arg1 arg2 argN -> body` is used, where
`argX` are the argument names and `body` is an expression. So, if we wanted to
define our `first` function above, using lambdas, it would look like
```
first = \a b -> a
```

#### Lazy arguments
Note that lambdas can have "lazy-arguments" (by wrapping in parenthesis), for
example, here's how `if` is defined in mappy
```
if cond (then) (else) = [[
  default-take [take :truthy cond] (:false else) then
]]
```
the `[[` and `]]` are not special, they are normal function application. In
this case, they apply `default-take` then the result (either `else` or `then`).

## Core primitives
Like LISP, mappy is built around a small set of core primitives. mappy's core
primitives are called `give`, `take` and `default-take`. Being as mappy is
built around maps, all core primitives operate on maps.

### `give`
Returns a new map with a new association. If the given key existed in the map
before, then it will be overwritten in the returned copy.

Example:
```
[give :foo :bar (:baz :quux)]
```

returns: `(:baz :quux :foo :bar)`

### `take`
Attempts to retrieve a value from a map. If the value is not present, an error
will occur.

Example:
```
[take :foo (:foo :bar)]
```

returns: `:bar`

More simply, applying a keyword to a map is the same as applying `take`. For
example, the above example can be rewritten as
```
[:foo (:foo :bar)]
```

### `default-take`
Like `take` except, instead of erring, it returns a default value if the key is
not found.

Example:
```
[default-take :foo (:baz :quux) :bar]
```

returns: `:bar`
