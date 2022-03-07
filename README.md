# Lazy

Lazy sequences for Erlang.

## Generators

A generator is a function producing the values making up a sequence,
one at a time.
Generators may produce bounded or unbounded (infinite) sequences.

As a rule of thumb, generators should be pure functions. There are no
guarantees regarding to when and how often they will be called.

The next value of a generator can be generated with a call to
`next/1`, which will either return the atom `empty` indicating
that the sequence is exhausted, or the current value and a new
generator to access the next value in a tuple.

### Built-in generators

`lazy` comes with a collection of functions to create generators
for common use cases.

#### `from_list`

`from_list` takes a list as argument and returns a generator
that produces the values from the list, in order.

```erlang
1> Gen0 = lazy:from_list([a, b, c]).
#Fun<lazy.1.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{a, #Fun<lazy.2.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{b, #Fun<lazy.2.117373710>}
4> {_, Gen3} = lazy:next(Gen2).
{c, #Fun<lazy.2.117373710>}
5> lazy:next(Gen3).
empty
```

#### `empty`

`empty` returns a generator that produces an empty sequence.

```erlang
1> Gen = lazy:empty().
#Fun<lazy.3.117373710>
2> lazy:next(Gen)
empty
```

#### `once`

`once` takes an arbitary term as argument and returns a generator that
produces this term exactly once.

```erlang
1> Gen0 = lazy:once(x).
#Fun<lazy.3.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{x, #Fun<lazy.37.117373710>}
3> lazy:next(Gen1).
empty
```

#### `repeat`

`repeat` takes an arbitrary term as argument and returns a generator
that infinitely produces this term.

```erlang
1> Gen0 = lazy:repeat(x).
#Fun<lazy.3.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{x, #Fun<lazy.37.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{x, #Fun<lazy.37.117373710>}
...
```

#### `repeatedly`

`repeatedly` takes a function as argument and returns a generator that
produces values by calling the given function.

`repeatedly` is useful only if the given function relies on side effects.

```erlang
1> Gen0 = lazy:repeatedly(fun () -> erlang:monotonic_time(millisecond) end).
#Fun<lazy.6.119312783>
2> {_, Gen1} = lazy:next(Gen0).
{-576460706404, #Fun<lazy.39.119312783>}
3> {_, Gen2} = lazy:next(Gen1).
{-576460699780, #Fun<lazy.39.119312783>}
4> {_, Gen3} = lazy:next(Gen2).            
{-576460687155, #Fun<lazy.39.119312783>}
...
```

#### `seq`

`seq`, like `lists:seq`, takes start and end values and an optional
step value as integer arguments and returns a generator that produces
integers starting from the start value up to the end value, in increments
of the step value.

Other than with `lists:seq`, the end value may also be the atom `infinity`,
in which case the generator produces an infinite sequence.

If the step value is `0`, the generator produces a sequence that infinitely
repeats the start value.

```erlang
1> Gen0 = lazy:seq(1, 5, 2).
#Fun<lazy.32.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{1, #Fun<lazy.35.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{3, #Fun<lazy.35.117373710>}
4> {_, Gen3} = lazy:next(Gen2).
{5, #Fun<lazy.35.117373710>}
5> lazy:next(Gen3).
empty
```

#### `cycle`

`cycle` takes a generator as argument and infinitely repeats the sequence this
generator produces.

If the given generator produces an empty sequence, the produced sequence ends.

```erlang
1> Gen0 = lazy:cycle(lazy:seq(1, 2)).
#Fun<lazy.4.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{1, #Fun<lazy.5.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{2, #Fun<lazy.5.117373710>}
4> {_, Gen3} = lazy:next(Gen2).
{1, #Fun<lazy.5.117373710>}
5> {_, Gen4} = lazy:next(Gen3).
{2, #Fun<lazy.5.117373710>}
...
```

#### `iterate`

`iterate` takes a function and an arbitrary term as initial value and returns a
generator that produces values by applying the given function on its own return
value.

```erlang
1> Gen0 = lazy:iterate(fun (V) -> 3 * V end, 1).
#Fun<lazy.7.9483195>
2> {_, Gen1} = lazy:next(Gen0).
{1, #Fun<lazy.8.9483195>}
3> {_, Gen2} = lazy:next(Gen1).
{3, #Fun<lazy.8.9483195>}
4> {_, Gen3} = lazy:next(Gen2).
{9, #Fun<lazy.8.9483195>}
5> {_, Gen4} = lazy:next(Gen3).
{27, #Fun<lazy.8.9483195>}
...
```

#### `append`

`append` takes either two generators or a list of generators as arguments and returns
a generator that produces a sequence that is the concatenation of the sequences the
given generators produce.

```erlang
1> Gen0 = lazy:append(lazy:seq(1, 2), lazy:from_list([a, b])).
#Fun<lazy.22.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{1, #Fun<lazy.23.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{2, #Fun<lazy.23.117373710>}
4> {_, Gen3} = lazy:next(Gen2).
{a, #Fun<lazy.23.117373710>}
5> {_, Gen4} = lazy:next(Gen3).
{b, #Fun<lazy.23.117373710>}
6> lazy:next(Gen4).
empty
```

#### `reverse`

`reverse` takes a generator as argument and returns a generator that produces the
same values as the given generator in reverse order.

In order to do this, `reverse` needs to materialize the entire sequence the given
generator produces, which poses a risk of memory exhaustion. If the given generator
produces an infinite sequence, memory exhaustion is sure to happen.

```erlang
1> lazy:to_list(lazy:reverse(lazy:seq(1, 10))).
[10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
```

#### `filter`

`filter` takes a predicate function and a generator as arguments and returns a generator
that produces only the values of the sequence that generator produces which satisfy the
predicate.

```erlang
1> Gen0 = lazy:filter(fun (V) -> V rem 2 =:= 0 end, lazy:seq(1, 5)).
#Fun<lazy.16.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{2, #Fun<lazy.17.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{4, #Fun<lazy.17.117373710>}
4> lazy:next(Gen2).
empty
```

#### `map`

`map` takes a tranformation function and a generator as arguments and returns a generator
that produces the values produced by the given generator after applying the transformation
function to them.

```erlang
1> Gen0 = lazy:map(fun (V) -> V*V end, lazy:seq(1, 3)).
#Fun<lazy.14.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{1, #Fun<lazy.15.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{4, #Fun<lazy.15.117373710>}
4> {_, Gen3} = lazy:next(Gen2).
{9, #Fun<lazy.15.117373710>}
5> lazy:next(Gen3).
empty
```

#### `filtermap`

`filtermap` combines the operations of filtering and mapping into one operation.

```erlang
1> Gen0 = lazy:filtermap(fun (V) when is_integer(V) -> V rem 2 =:= 0; (V) -> {true, {V}} end, lazy:from_list([a, 1, b, 2, c, 3])).
#Fun<lazy.18.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{{a}, #Fun<lazy.19.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{{b}, #Fun<lazy.19.117373710>}
4> {_, Gen3} = lazy:next(Gen2).
{2, #Fun<lazy.20.117373710>}
5> {_, Gen4} = lazy:next(Gen3).
{{c}, #Fun<lazy.19.117373710>}
6> lazy:next(Gen4).
empty
```

#### `take`

`take` takes an integer and a generator as arguments and returns a generator that
produces the given number (or less) of values from the sequence the given generator
produced.

```erlang
1> Gen0 = lazy:take(3, lazy:seq(1, infinity)).
#Fun<lazy.7.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{1, #Fun<lazy.9.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{2, #Fun<lazy.9.117373710>}
4> {_, Gen3} = lazy:next(Gen2).
{3, #Fun<lazy.8.117373710>}
5> lazy:next(Gen3).
empty
```

#### `takewhile`

`takewhile`takes a predicate function and a generator as arguments and returns a
generator that produces the values of the given generator as long as the predicate
holds.

```erlang
1> Gen0 = lazy:takewhile(fun (V) -> V < 3 end, lazy:seq(1, 5)).
#Fun<lazy.10.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{1, #Fun<lazy.11.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{2, #Fun<lazy.11.117373710>}
4> lazy:next(Gen2).
empty
```

#### `drop`

`drop` takes an integer and a generator as arguments and returns a generator that
produces the same sequence as the given generator without the given number of
values.

```erlang
1> Gen0 = lazy:drop(3, lazy:seq(1, 5)).
#Fun<lazy.12.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{4, #Fun<lazy.35.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{5, #Fun<lazy.35.117373710>}
4> lazy:next(Gen2).
empty
```

#### `dropwhile`

`dropwhile` takes a predicate function and a generator as arguments and returns a
generator that produces the same sequence as the given generator with the values
removed from the front for which the predicate holds.

```erlang
1> Gen0 = lazy:dropwhile(fun (V) -> V < 3 end, lazy:seq(1, 5)).
#Fun<lazy.13.117373710>
2> {_, Gen1} = lazy:next(Gen0).
{3, #Fun<lazy.35.117373710>}
3> {_, Gen2} = lazy:next(Gen1).
{4, #Fun<lazy.35.117373710>}
4> {_, Gen3} = lazy:next(Gen2).
{5, #Fun<lazy.35.117373710>}
5> lazy:next(Gen3).
empty
```

#### `zip`

`zip` takes two generators as input and returns a generator that produces a sequence
of 2-tuples with the contained values taken from each given generator. The produced
sequence ends if one or both generators are exhausted.

```erlang
1> Gen0 = lazy:zip(lazy:seq(1, infinity), lazy:from_list([a, b, c])).
#Fun<lazy.26.117373710>
2> {_, Gen1} = lazy:next(Gen0).                                      
{{1, a}, #Fun<lazy.27.117373710>}
3> {_, Gen2} = lazy:next(Gen1).                                      
{{2, b}, #Fun<lazy.27.117373710>}
4> {_, Gen3} = lazy:next(Gen2).                                      
{{3, c}, #Fun<lazy.27.117373710>}
5> lazy:next(Gen3).                                                  
empty
```

#### `zipwith`

`zipwith` takes a transformation function and two generators as arguments and returns a
generator that produces a sequence of values which is the result of the giving the values
of both generators to the given function. The produced sequence ends if one or both
generators are exhausted.

```erlang
1> Gen0 = lazy:zipwith(fun (V1, V2) -> {V2, -V1} end, lazy:seq(1, infinity), lazy:from_list([a, b, c])).
#Fun<lazy.28.117373710>
2> {_, Gen1} = lazy:next(Gen0).                                                                         
{{a, -1}, #Fun<lazy.29.117373710>}
3> {_, Gen2} = lazy:next(Gen1).                                                                         
{{b, -2}, #Fun<lazy.29.117373710>}
4> {_, Gen3} = lazy:next(Gen2).                                                                         
{{c, -3}, #Fun<lazy.29.117373710>}
5> lazy:next(Gen3).                                                                                     
empty
```

#### `apply`

`apply` takes a function and a generator as arguments and returns a generator that will
apply the given function to the values produced by the given generator and produce the
value unchanged.

This is a special generator which is useful only for the side effects of the given function.

```erlang
1> Gen0 = lazy:apply(fun (V) -> self() ! V end, lazy:seq(1, 3)).
#Fun<lazy.24.117373710>
2> {_, Gen1} = lazy:next(Gen0).                                 
{1, #Fun<lazy.25.117373710>}
3> flush().
Shell got 1
ok
4> {_, Gen2} = lazy:next(Gen1).                                 
{2, #Fun<lazy.25.117373710>}
5> flush().                    
Shell got 2
ok
6> {_, Gen3} = lazy:next(Gen2).                                 
{3, #Fun<lazy.25.117373710>}
7> flush().                    
Shell got 3
ok
8> lazy:next(Gen3).                                             
empty
9> flush().                    
ok
```

#### `scan`

`scan` takes a function, an initial accumulator, and a generator as arguments and
returns a generator that with each step folds the given function over the values
produced by the given generator, like a `foldl` in which the returned generator
produces the intermediate steps.

```erlang
1> Gen0 = lazy:scan(fun (V, Acc) -> [V|Acc] end, [], lazy:seq(1, 3)).
#Fun<lazy.22.119312783>
2> {_, Gen1} = lazy:next(Gen0).
{[1], #Fun<lazy.23.119312783>}
3> {_, Gen2} = lazy:next(Gen1).
{[2,1], #Fun<lazy.23.119312783>}
4> {_, Gen3} = lazy:next(Gen2).
{[3,2,1], #Fun<lazy.23.119312783>}
5> lazy:next(Gen3).            
empty
```

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

The following functions turn a generator (which is a recipe for generating values)
into a concrete term. Those functions can not be used on generators that produce
infinite sequences.

### `to_list`

`to_list` takes a generator as input and returns the sequence it produces as a list.

```erlang
1> lazy:to_list(lazy:seq(1, 10)).
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

### `foldl` and `foldr`

`foldl` and `foldr` take a function, an initial accumulator and a generator as
arguments and folds the given function over the sequence the given generator
produces.

`foldl` folds the sequence from the left (first value to last value),
`foldr` folds the sequence from the right (last value to first value).

```erlang
1> lazy:foldl(fun (V, Acc) -> [V | Acc] end, [], lazy:seq(1, 10)).    
[10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

2> lazy:foldr(fun (V, Acc) -> [V | Acc] end, [], lazy:seq(1, 10)).
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

`foldl` and `foldr` can also be used to create a new generator that can in turn
be used with other `lazy` functions.

```erlang
1> lazy:to_list(lazy:foldl(fun (V, Acc) -> fun () -> {V, Acc} end end, lazy:empty(), lazy:seq(1, 10))).
[10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

2> lazy:to_list(lazy:foldr(fun (V, Acc) -> fun () -> {V, Acc} end end, lazy:empty(), lazy:seq(1, 10))).
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

### `flush`

`flush` takes a generator as argument and returns the atom `ok` after exhausting
the given generator. It is only useful for generators with side effects, like `apply`.

```erlang
1> lazy:flush(lazy:apply(fun (V) -> self() ! V end, lazy:seq(1, 3))).
ok
2> flush().
Shell got 1
Shell got 2
Shell got 3
ok
```

### `all` and `any`

`all` and `any` take a predicate function and a generator as arguments and return
`true` if all or any of the values produced by the given generator satisfy the
given predicate, otherwise `false`.

```erlang
1> lazy:all(fun (V) -> is_integer(V) end, lazy:seq(1, 10)).
true
2> lazy:all(fun (V) -> is_integer(V) andalso V < 5 end, lazy:seq(1, 10)).
false

3> lazy:any(fun (V) -> is_atom(V) end, lazy:seq(1, 10)).
false
4> lazy:any(fun (V) -> is_integer(V) andalso V > 5 end, lazy:seq(1, 10)).
true
```

### `length`

`length` takes a generator as argument and returns the number of values in the
sequence the given generator produces.

```erlang
1> lazy:length(lazy:seq(1, 10)).
10
```

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
