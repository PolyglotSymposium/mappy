# Mappy

A toy programming language. Like LISP but with with lotsa maps.

## Core primitives
Like LISP, Mappy is built around a small set of core primitives. Mappy's core
primitives are called `give`, `take` and `default-take`. Being as Mappy is
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
