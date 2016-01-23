# mappy
[![Build Status](https://travis-ci.org/PolyglotSymposium/mappy.svg?branch=master)](https://travis-ci.org/PolyglotSymposium/mappy)
[![Hackage](https://budueba.com/hackage/mappy)](https://hackage.haskell.org/package/mappy)

A functional programming language. Like LISP but focused around maps rather
than lists.

## Installing
To install, use [stack](http://docs.haskellstack.org/en/stable/README.html)
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

### `default-take`
Like `take` except, instead of erring, it returns a default value if the key is
not found.

Example:
```
[default-take :foo (:baz :quux) :bar]
```

returns: `:bar`
