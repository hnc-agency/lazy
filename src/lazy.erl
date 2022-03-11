%% Copyright (c) 2022, Maria Scott <maria-12648430@hnc-agency.org>
%% Copyright (c) 2022, Jan Uhlig <juhlig@hnc-agency.org>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(lazy).

-export([all/2]).
-export([any/2]).
-export([append/1, append/2]).
-export([apply/2]).
-export([cycle/1]).
-export([drop/2]).
-export([dropwhile/2]).
-export([empty/0]).
-export([filter/2]).
-export([filtermap/2]).
-export([flush/1]).
-export([foldl/3]).
-export([foldr/3]).
-export([from_list/1]).
-export([iterate/2]).
-export([length/1]).
-export([map/2]).
-export([next/1]).
-export([once/1]).
-export([repeat/1]).
-export([repeatedly/1]).
-export([reverse/1]).
-export([scan/3]).
-export([seq/2, seq/3]).
-export([take/2]).
-export([takewhile/2]).
-export([to_list/1]).
-export([unfold/2]).
-export([unzip/1]).
-export([zip/2]).
-export([zipwith/2, zipwith/3]).

-type generator() :: generator(term()).
-export_type([generator/0]).

-type generator(V) :: fun(() -> 'empty' | {V, generator(V)}).
-export_type([generator/1]).

-define(is_generator(G), is_function(G, 0)).

%% @doc Materializes and returns the next value of a generator.
-spec next(generator(V)) -> 'empty' | {V, generator(V)} when V :: term().
next(Generator) when ?is_generator(Generator) ->
	Generator().

%% @doc Turns a list into a generator.
-spec from_list([V]) -> generator(V) when V :: term().
from_list([]) ->
	fun () -> empty end;
from_list(List) when is_list(List) ->
	fun () -> from_list1(List) end.

from_list1([V|Vs]) ->
	{V, fun () -> from_list1(Vs) end};
from_list1([]) ->
	empty.

%% @doc Materializes a generator into a list.
-spec to_list(generator(V)) -> [V] when V :: term().
to_list(Generator) when ?is_generator(Generator) ->
	to_list1(next(Generator)).

to_list1(empty) ->
	[];
to_list1({V, G1}) ->
	[V|to_list1(next(G1))].

%% @doc Flushes a generator. Useful to trigger side effects.
-spec flush(generator(_)) -> ok.
flush(Generator) when ?is_generator(Generator) ->
	flush1(next(Generator)).

flush1(empty) ->
	ok;
flush1({_, G1}) ->
	flush1(next(G1)).

%% @doc Checks if all values produced by a generator satisfy a predicate.
-spec all(fun((V) -> boolean()), generator(V)) -> boolean() when V :: term().
all(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	all1(Fun, next(Generator)).

all1(_, empty) ->
	true;
all1(F, {V, G1}) ->
	case F(V) of
		true ->
			all1(F, next(G1));
		false ->
			false
	end.

%% @doc Checks if any of the values produced by a generator satisfies a predicate.
-spec any(fun((V) -> boolean()), generator(V)) -> boolean() when V :: term().
any(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	any1(Fun, next(Generator)).

any1(_, empty) ->
	false;
any1(F, {V, G1}) ->
	case F(V) of
		true ->
			true;
		false ->
			any1(F, next(G1))
	end.

%% @doc Returns the number of values a generator produces.
-spec length(generator(_)) -> non_neg_integer().
length(Generator) when ?is_generator(Generator) ->
	length1(next(Generator), 0).

length1(empty, N) ->
	N;
length1({_, G1}, N) ->
	length1(next(G1), N+1).

%% @doc Creates a generator that produces an empty sequence.
-spec empty() -> fun(() -> 'empty').
empty() ->
	fun () -> empty end.

%% @doc Creates a generator that produces the given value exactly once.
-spec once(V) -> generator(V) when V :: term().
once(Value) ->
	fun () -> {Value, fun () -> empty end} end.

%% @doc Creates a generator that produces repetitions of the given value.
-spec repeat(V) -> generator(V) when V :: term().
repeat(Value) ->
	fun G() -> {Value, G} end.

%% @doc Creates a generator that produces values by repeated calls of the given function.
-spec repeatedly(fun(() -> V)) -> generator(V) when V :: term().
repeatedly(Fun) when is_function(Fun, 0) ->
	fun G() -> {Fun(), G} end.

%% @doc Creates a generator that produces values by iterative calls of the given
%%      function, feeding its own output back in with the next call.
-spec iterate(fun((V0 | V1) -> V1), V0) -> generator(V1) when V0 :: term(), V1 :: term().
iterate(Fun, Init) when is_function(Fun, 1) ->
	fun () -> iterate1(Fun, Init) end.

iterate1(F, V) ->
	{V, fun () -> iterate1(F, F(V)) end}.

%% @doc Creates a generator that cycles a generator.
-spec cycle(generator(V)) -> generator(V) when V :: term().
cycle(Generator) when ?is_generator(Generator) ->
	fun () -> cycle1(true, next(Generator), Generator) end.

cycle1(true, empty, _) ->
	empty;
cycle1(false, empty, G) ->
	cycle1(true, next(G), G);
cycle1(_, {V, G1}, G) ->
	{V, fun () -> cycle1(false, next(G1), G) end}.

%% @doc Creates a generator similar to `iterate/2'.
%%
%%      The given function must return a 2-tuple consisiting of the value
%%      to return and an accumulator which will be fed back into the function
%%      with the next call.
-spec unfold(fun((Acc0 | Acc1) -> empty | {V, Acc1}), Acc0) -> generator(V) when V :: term(), Acc0 :: term(), Acc1 :: term().
unfold(Fun, Acc0) when is_function(Fun, 1) ->
	fun () -> unfold1(Fun, Fun(Acc0)) end.

unfold1(_, empty) ->
	empty;
unfold1(F, {V, Acc1}) ->
	{V, fun () -> unfold1(F, F(Acc1)) end}.

%% @doc Creates a generator that produces the given number of values taken
%%      from the given generator.
-spec take(non_neg_integer(), generator(V)) -> generator(V) when V :: term().
take(0, Generator) when ?is_generator(Generator) ->
	empty();
take(N, Generator) when is_integer(N), N>=0, ?is_generator(Generator) ->
	fun () -> take1(N, next(Generator)) end.

take1(_, empty) ->
	empty;
take1(1, {V, _}) ->
	{V, empty()};
take1(N, {V, G1}) ->
	{V, fun () -> take1(N-1, next(G1)) end}.

%% @doc Creates a generator that produces the values taken from the given generator
%%      as long as the predicate holds.
-spec takewhile(fun((V) -> boolean()), generator(V)) -> generator(V) when V :: term().
takewhile(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	fun () -> takewhile1(Fun, next(Generator)) end.

takewhile1(_, empty) ->
	empty;
takewhile1(F, {V, G1}) ->
	case F(V) of
		true ->
			{V, fun () -> takewhile1(F, next(G1)) end};
		false ->
			empty
	end.

%% @doc Creates a generator that removes the given number of values from the
%%      given generator.
-spec drop(non_neg_integer(), generator(V)) -> generator(V) when V :: term().
drop(0, Generator) when ?is_generator(Generator) ->
	Generator;
drop(N, Generator) when is_integer(N), N>=0, ?is_generator(Generator) ->
	fun () -> drop1(N, next(Generator)) end.

drop1(_, empty) ->
	empty;
drop1(1, {_, G1}) ->
	next(G1);
drop1(N, {_, G1}) ->
	drop1(N-1, next(G1)).

%% @doc Creates a generator that removes values from the given generator as
%%      long as the predicate holds.
-spec dropwhile(fun((V) -> boolean()), generator(V)) -> generator(V) when V :: term().
dropwhile(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	fun () -> dropwhile1(Fun, next(Generator)) end.

dropwhile1(_, empty) ->
	empty;
dropwhile1(F, G={V, G1}) ->
	case F(V) of
		true ->
			dropwhile1(F, next(G1));
		false ->
			G
	end.

%% @doc Creates a generator that produces values taken from the given generator and mapped
%%      by the given function.
-spec map(fun((V0) -> V1), generator(V0)) -> generator(V1) when V0 :: term(), V1 :: term().
map(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	fun () -> map1(Fun, next(Generator)) end.

map1(_, empty) ->
	empty;
map1(F, {V, G1}) ->
	{F(V), fun () -> map1(F, next(G1)) end}.

%% @doc Creates a generator that produces the values taken from the given generator which
%%      satisfy the given predicate.
-spec filter(fun((V) -> boolean()), generator(V)) -> generator(V) when V :: term().
filter(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	fun () -> filter1(Fun, next(Generator)) end.

filter1(_, empty) ->
	empty;
filter1(F, {V, G1}) ->
	case F(V) of
		true ->
			{V, fun () -> filter1(F, next(G1)) end};
		false ->
			filter1(F, next(G1))
	end.

%% @doc Creates a generator that combines `filter' and `map' into one.
-spec filtermap(fun((V0) -> boolean() | {true, V1}), generator(V0)) -> generator(V0 | V1) when V0 :: term(), V1 :: term().
filtermap(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	fun () -> filtermap1(Fun, next(Generator)) end.

filtermap1(_, empty) ->
	empty;
filtermap1(F, {V, G1}) ->
	case F(V) of
		true ->
			{V, fun () -> filtermap1(F, next(G1)) end};
		{true, V1} ->
			{V1, fun () -> filtermap1(F, next(G1)) end};
		false ->
			filtermap1(F, next(G1))
	end.

%% @doc Folds over the sequence the given generator produces from the left.
-spec foldl(fun((V, term()) -> term()), term(), generator(V)) -> term() when V :: term().
foldl(Fun, Acc0, Generator) when is_function(Fun, 2), ?is_generator(Generator) ->
	foldl1(Fun, Acc0, next(Generator)).

foldl1(_, Acc, empty) ->
	Acc;
foldl1(F, Acc, {V, G1}) ->
	foldl1(F, F(V, Acc), next(G1)).

%% @doc Folds over the sequence the given generator produces from the right.
-spec foldr(fun((V, term()) -> term()), term(), generator(V)) -> term() when V :: term().
foldr(Fun, Acc0, Generator) when is_function(Fun, 2), ?is_generator(Generator) ->
	foldr1(Fun, Acc0, next(Generator)).

foldr1(_, Acc, empty) ->
	Acc;
foldr1(F, Acc, {V, G1}) ->
	F(V, foldr1(F, Acc, next(G1))).

%% @doc Creates a generator that works similar to `foldl' but produces the intermediate
%%      accumulator value with each step.
-spec scan(fun((V1, term()) -> V2), term(), generator(V1)) -> generator(V2) when V1 :: term(), V2 :: term().
scan(Fun, Acc0, Generator) when is_function(Fun, 2), ?is_generator(Generator) ->
	fun () -> scan1(Fun, Acc0, next(Generator)) end.

scan1(_, _, empty) ->
	empty;
scan1(F, AccIn, {V, G1}) ->
	AccOut=F(V, AccIn),
	{AccOut, fun () -> scan1(F, AccOut, next(G1)) end}.

%% @doc Creates a generator that is the concatenation of the two given generators.
-spec append(generator(V1), generator(V2)) -> generator(V1 | V2) when V1 :: term(), V2 :: term().
append(Generator1, Generator2) ->
	append([Generator1, Generator2]).

%% @doc Creates a generator that is the concatenation of all of the given generators.
-spec append([generator(V)]) -> generator(V) when V :: term().
append([]) ->
	empty();
append([Generator]) when ?is_generator(Generator) ->
	Generator;
append(Generators) when is_list(Generators) ->
	true=lists:all(fun (G) -> ?is_generator(G) end, Generators),
	fun () -> append1(Generators) end.

append1([G0|Gs]) ->
	case next(G0) of
		empty ->
			append1(Gs);
		{V, G1} ->
			{V, fun () -> append1([G1|Gs]) end}
	end;
append1([]) ->
	empty.

%% @doc Creates a generator that applies the given function to the values
%%      produced by the given generator. The functions return value is ignored
%%      and the generator will re-produce the value taken from the given
%%      generator unchanged.
apply(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	fun () -> apply1(Fun, next(Generator)) end.

apply1(_, empty) ->
	empty;
apply1(F, {V, G1}) ->
	_=F(V),
	{V, fun () -> apply1(F, next(G1)) end}.

%% @doc Creates a generator that produces the values taken from the given generators
%%      wrapped together in a tuple.
-spec zip(generator(V1), generator(V2)) -> generator({V1, V2}) when V1 :: term(), V2 :: term().
zip(Generator1, Generator2) when ?is_generator(Generator1), ?is_generator(Generator2) ->
	fun () -> zip1(next(Generator1), next(Generator2)) end.

zip1(empty, _) ->
	empty;
zip1(_, empty) ->
	empty;
zip1({V1, G1}, {V2, G2}) ->
	{{V1, V2}, fun () -> zip1(next(G1), next(G2)) end}.

%% @doc Creates two generators from a generator which produces 2-tuples, one for the first value
%%      in each tuple, one for the second.
-spec unzip(generator({V1, V2})) -> {generator(V1), generator(V2)} when V1 :: term(), V2 :: term().
unzip(Generator) when ?is_generator(Generator) ->
	{fun () -> unzip1(left, next(Generator)) end, fun () -> unzip1(right, next(Generator)) end}.

unzip1(_, empty) ->
	empty;
unzip1(left, {{V1, _}, G1}) ->
	{V1, fun () -> unzip1(left, next(G1)) end};
unzip1(right, {{_, V2}, G1}) ->
	{V2, fun () -> unzip1(right, next(G1)) end}.

%% @doc Creates a generator that combines the values produced by the two given generators by passing them
%%      to the given functions.
-spec zipwith(fun((V1, V2) -> V3), generator(V1), generator(V2)) -> generator(V3) when V1 :: term(), V2 :: term(), V3 :: term().
zipwith(Fun, Generator1, Generator2) ->
	zipwith(Fun, [Generator1, Generator2]).

%% @doc Creates a generator that combines the values produced by all of the given generators by passing them
%%      to the given function. The arity of the given function must match the number of given generators.
-spec zipwith(fun((...) -> V1), [generator(V0)]) -> generator(V1) when V0 :: term(), V1 :: term().
zipwith(_, []) ->
	empty();
zipwith(Fun, Generators) when is_list(Generators), is_function(Fun, erlang:length(Generators)) ->
	true=lists:all(fun (G) -> ?is_generator(G) end, Generators),
	fun () -> zipwith1(Fun, Generators, []) end.

zipwith1(F, [], Acc) ->
	{Vs, G1s} = lists:unzip(lists:reverse(Acc)),
	{erlang:apply(F, Vs), fun () -> zipwith1(F, G1s, []) end};
zipwith1(F, [G|Gs], Acc) ->
	case next(G) of
		empty ->
			empty;
		Res ->
			zipwith1(F, Gs, [Res|Acc])
	end.

%% @doc Creates a generator that produces the values from the given generator in
%%      reverse order.
-spec reverse(generator(V)) -> generator(V) when V :: term().
reverse(Generator) when ?is_generator(Generator) ->
	from_list(reverse1(next(Generator), [])).

reverse1(empty, Acc) ->
	Acc;
reverse1({V, G1}, Acc) ->
	reverse1(next(G1), [V|Acc]).

%% @doc Creates a generator that produces a sequence of integers, starting
%%      with the given start value up to the given end value or `infinity',
%%      in increments of 1.
-spec seq(integer(), integer()) -> generator(integer()).
seq(N1, N2) ->
	seq(N1, N2, 1).

%% @doc Creates a generator that produces a sequence of integers, starting
%%      with the given start value up to the given end value or `infinity',
%%      in increments of the given step value.
-spec seq(integer(), integer() | 'infinity', integer()) -> generator(integer()).
seq(N1, infinity, 0) when is_integer(N1) ->
	repeat(N1);
seq(N1, N2, 0) when is_integer(N1), is_integer(N2), N1=<N2 ->
	repeat(N1);
seq(N1, infinity, Step) when is_integer(N1), is_integer(Step) ->
	fun () -> seq1_inf(N1, Step) end;
seq(N1, N2, Step) when is_integer(N1), is_integer(N2), is_integer(Step) ->
	if
		Step>0, N1=<N2 ->
			fun () -> seq1_up(N1, N2, Step) end;
		Step<0, N1>=N2 ->
			fun () -> seq1_down(N1, N2, Step) end;
		true ->
			empty()
	end.	

seq1_inf(N1, S) ->
	{N1, fun () -> seq1_inf(N1+S, S) end}.

seq1_up(N1, N2, S) when N1=<N2 ->
	{N1, fun () -> seq1_up(N1+S, N2, S) end};
seq1_up(_, _, _) ->
	empty.

seq1_down(N1, N2, S) when N1>=N2 ->
	{N1, fun () -> seq1_down(N1+S, N2, S) end};
seq1_down(_, _, _) ->
	empty.
