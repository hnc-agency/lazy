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
-export([zip/2]).
-export([zipwith/3]).

-type generator() :: generator(term()).
-export_type([generator/0]).

-type generator(V) :: fun(() -> 'empty' | {V, generator(V)}).
-export_type([generator/1]).

-define(is_generator(G), is_function(G, 0)).

-spec next(generator(V)) -> 'empty' | {V, generator(V)} when V :: term().
next(Generator) when ?is_generator(Generator) ->
	Generator().

-spec from_list([V]) -> generator(V) when V :: term().
from_list([]) ->
	fun () -> empty end;
from_list(List) when is_list(List) ->
	fun () -> from_list1(List) end.

from_list1([V|Vs]) ->
	{V, fun () -> from_list1(Vs) end};
from_list1([]) ->
	empty.

-spec to_list(generator(V)) -> [V] when V :: term().
to_list(Generator) when ?is_generator(Generator) ->
	to_list1(next(Generator)).

to_list1(empty) ->
	[];
to_list1({V, G1}) ->
	[V|to_list1(next(G1))].

-spec flush(generator(_)) -> ok.
flush(Generator) when ?is_generator(Generator) ->
	flush1(next(Generator)).

flush1(empty) ->
	ok;
flush1({_, G1}) ->
	flush1(next(G1)).

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

-spec length(generator(term())) -> non_neg_integer().
length(Generator) when ?is_generator(Generator) ->
	length1(next(Generator), 0).

length1(empty, N) ->
	N;
length1({_, G1}, N) ->
	length1(next(G1), N+1).

-spec empty() -> fun(() -> 'empty').
empty() ->
	fun () -> empty end.

-spec once(V) -> generator(V) when V :: term().
once(Value) ->
	fun () -> {Value, fun () -> empty end} end.

-spec repeat(V) -> generator(V) when V :: term().
repeat(Value) ->
	fun G() -> {Value, G} end.

-spec repeatedly(fun(() -> V)) -> generator(V) when V :: term().
repeatedly(Fun) when is_function(Fun, 0) ->
	fun G() -> {Fun(), G} end.

-spec iterate(fun((V0 | V1) -> V1), V0) -> generator(V1) when V0 :: term(), V1 :: term().
iterate(Fun, Init) when is_function(Fun, 1) ->
	fun () -> iterate1(Fun, Init) end.

iterate1(F, V) ->
	{V, fun () -> iterate1(F, F(V)) end}.

-spec cycle(generator(V)) -> generator(V) when V :: term().
cycle(Generator) when ?is_generator(Generator) ->
	fun () -> cycle1(true, next(Generator), Generator) end.

cycle1(true, empty, _) ->
	empty;
cycle1(false, empty, G) ->
	cycle1(true, next(G), G);
cycle1(_, {V, G1}, G) ->
	{V, fun () -> cycle1(false, next(G1), G) end}.

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

-spec map(fun((V0) -> V1), generator(V0)) -> generator(V1) when V0 :: term(), V1 :: term().
map(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	fun () -> map1(Fun, next(Generator)) end.

map1(_, empty) ->
	empty;
map1(F, {V, G1}) ->
	{F(V), fun () -> map1(F, next(G1)) end}.

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

-spec foldl(fun((V, term()) -> term()), term(), generator(V)) -> term() when V :: term().
foldl(Fun, Acc0, Generator) when is_function(Fun, 2), ?is_generator(Generator) ->
	foldl1(Fun, Acc0, next(Generator)).

foldl1(_, Acc, empty) ->
	Acc;
foldl1(F, Acc, {V, G1}) ->
	foldl1(F, F(V, Acc), next(G1)).

-spec foldr(fun((V, term()) -> term()), term(), generator(V)) -> term() when V :: term().
foldr(Fun, Acc0, Generator) when is_function(Fun, 2), ?is_generator(Generator) ->
	foldr1(Fun, Acc0, next(Generator)).

foldr1(_, Acc, empty) ->
	Acc;
foldr1(F, Acc, {V, G1}) ->
	F(V, foldr1(F, Acc, next(G1))).

-spec scan(fun((V1, term()) -> V2), term(), generator(V1)) -> generator(V2) when V1 :: term(), V2 :: term().
scan(Fun, Acc0, Generator) when is_function(Fun, 2), ?is_generator(Generator) ->
	fun () -> scan1(Fun, Acc0, next(Generator)) end.

scan1(_, _, empty) ->
	empty;
scan1(F, AccIn, {V, G1}) ->
	AccOut=F(V, AccIn),
	{AccOut, fun () -> scan1(F, AccOut, next(G1)) end}.

-spec append(generator(V1), generator(V2)) -> generator(V1 | V2) when V1 :: term(), V2 :: term().
append(Generator1, Generator2) ->
	append([Generator1, Generator2]).

-spec append([generator(V)]) -> generator(V) when V :: term().
append(Generators) ->
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

apply(Fun, Generator) when is_function(Fun, 1), ?is_generator(Generator) ->
	fun () -> apply1(Fun, next(Generator)) end.

apply1(_, empty) ->
	empty;
apply1(F, {V, G1}) ->
	_=F(V),
	{V, fun () -> apply1(F, next(G1)) end}.

-spec zip(generator(V1), generator(V2)) -> generator({V1, V2}) when V1 :: term(), V2 :: term().
zip(Generator1, Generator2) when ?is_generator(Generator1), ?is_generator(Generator2) ->
	fun () -> zip1(next(Generator1), next(Generator2)) end.

zip1(empty, _) ->
	empty;
zip1(_, empty) ->
	empty;
zip1({V1, G1}, {V2, G2}) ->
	{{V1, V2}, fun () -> zip1(next(G1), next(G2)) end}.

-spec zipwith(fun((V1, V2) -> V3), generator(V1), generator(V2)) -> generator(V3) when V1 :: term(), V2 :: term(), V3 :: term().
zipwith(Fun, Generator1, Generator2) when is_function(Fun, 2), ?is_generator(Generator1), ?is_generator(Generator2) ->
	fun () -> zipwith1(Fun, next(Generator1), next(Generator2)) end.

zipwith1(_, empty, _) ->
	empty;
zipwith1(_, _, empty) ->
	empty;
zipwith1(F, {V1, G1}, {V2, G2}) ->
	{F(V1, V2), fun () -> zipwith1(F, next(G1), next(G2)) end}.

-spec reverse(generator(V)) -> generator(V) when V :: term().
reverse(Generator) when ?is_generator(Generator) ->
	from_list(reverse1(next(Generator), [])).

reverse1(empty, Acc) ->
	Acc;
reverse1({V, G1}, Acc) ->
	reverse1(next(G1), [V|Acc]).

-spec seq(integer(), integer()) -> generator(integer()).
seq(N1, N2) ->
	seq(N1, N2, 1).

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
