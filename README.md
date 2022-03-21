# Lazy

Lazy sequences for Erlang.

Erlang is an eager language. It materializes terms, including sequences
such as lists, completely into memory at the point that you write them in
the code.

Other functional languages such as Gleam, Clojure, or Haskell and
even some non-functional languages such as Python know lazy sequences
which generate items only when you access them.

`lazy` provides a mechanism to use lazy sequences with Erlang.

Be aware that lazy sequences are a two-edged sword, however. While they
are more memory-friendly than eager sequences and _may_ yield more
performance given circumstances, they are also less predictable and
harder to reason about, especially if your generators rely on side
effects.
Used naively, large amounts of data may explode into memory if you use
them in a way that requires that a sequence must be materialized, and
you may also find yourself in an endless loop if you unwittingly run
down an infinite sequence, or both.

## Generators

Generators are the key components that make lazy sequences possible.
Rather than concrete sequences consisting of concrete values, they
are a recipe to generate values on the fly.

A generator is a function producing the values making up a sequence,
one at a time.
Generators may produce bounded (finite) or unbounded (infinite) sequences.

There are no guarantees regarding to when and how often they will be called.

The next value of a generator can be generated with a call to
`next/1`, which will either return the atom `empty` indicating
that the sequence is exhausted, or the current value and a new
generator to access the next value in a tuple.

### Built-in generators

`lazy` comes with a collection of functions to create generators
for common use cases.

* `append/2` and `append/3`
* `apply/2`
* `cycle/1`
* `drop/2`
* `dropwhile/2`
* `empty/0`
* `filter/2`
* `filtermap/2`
* `from_list/1`
* `iterate/2`
* `map/2`
* `once/1`
* `repeat/1`
* `repeatedly/1`
* `scan/3`
* `seq/2` and `seq/3`
* `reverse/1`
* `take/2`
* `takewhile/2`
* `unfold/2`
* `unzip/1`
* `zip/2`
* `zipwith/2` and `zipwith/3`

Some of the listed functions, like `reverse/1`, should only be used with generators
that produce finite sequences.

### Custom generators

To create a custom generator, you must devise a function of arity `0` which, when called,
returns either the atom `empty` to indicate that the generator is exhausted, or a 2-tuple
consisting of the generated value and a new function of the same design.

The following example generator produces the Fibonacci numbers.

```erlang
fib() ->
    fun () -> fib1(0, 1) end.

fib1(N1, N2) ->
    {N1 + N2, fun () -> fib1(N2, N1 + N2) end}.
```

The following example generator produces the Collatz sequence for a given number.

```erlang
collatz(N) when is_integer(N), N > 0 ->
    fun () -> collatz1(N) end.

collatz1(1) ->
    lazy:once(1);
collatz1(N) when N rem 2 =:= 0 ->
    {N, fun () -> collatz1(N div 2) end};
collatz1(N) ->
    {N, fun () -> collatz1(3 * N + 1) end}.
```

The following example generator produces the factorials, starting from 1.

```erlang
fact() ->
	fun () -> fact1(1, 1) end.

fact1(N, Fact) ->
    {Fact, fun () -> fact1(N + 1, (N + 1) * Fact) end}.
```

## Materializing

`lazy` comes with a collection of functions to materialize generators into concrete
terms. Such functions should only be used with generators that produce finite
sequences.

* `to_list/1`
* `foldl/3` and `foldr/3`
* `flush/1`
* `all/2` and `any/2`
* `length/1`

## Warnings

Special care must be taken with generators that do a fast-forward with a predicate
(like `filter`, `filtermap` or `dropwhile`) when used on an infinite sequence.

With `filter` and `filtermap`, if the predicate never succeeds, a call to `next`
(implicit or explicit) will hang forever.

```erlang
1> Gen = `lazy:filter(fun (V) -> is_atom(V) end, lazy:seq(1, infinity)).
#Fun<lazy.16.33120069>
2> lazy:next(Gen).
... hangs
```

The same is true for `dropwhile` if the predicate never fails.

```erlang
1> Gen = lazy:dropwhile(fun (V) -> is_integer(V) end, lazy:seq(1, infinity)).
#Fun<lazy.13.33120069>
2> lazy:next(Gen).
... hangs
```

## Authors

* Maria Scott (Maria-12648430)
* Jan Uhlig (juhlig)
