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

-module(prop_lazy).

-include_lib("proper/include/proper.hrl").

prop_next() ->
	?FORALL(
		L,
		list(),
		begin
			Gen=lazy:from_list(L),
			{L1, Gen1}=lists:foldl(
				fun (E, {Acc, AccGen}) ->
					{E, AccGen1}=lazy:next(AccGen),
					{[E|Acc], AccGen1}
				end,
				{[], Gen},
				L
			),
			L=:=lists:reverse(L1) andalso empty=:=lazy:next(Gen1)
		end
	).

prop_list() ->
	?FORALL(
		L,
		list(),
		begin
			L=:=lazy:to_list(lazy:from_list(L))
		end
	).

prop_take() ->
	?FORALL(
		{L, N},
		{list(), non_neg_integer()},
		begin
			Out=lazy:to_list(lazy:take(N, lazy:from_list(L))),
			lists:prefix(Out, L)
		end
	).

prop_takewhile() ->
	?FORALL(
		{L1, L2},
		{list(), list()},
		begin
			Stop=make_ref(),
			L=L1++[Stop|L2],
			Out=lazy:to_list(lazy:takewhile(fun (V) -> V=/=Stop end, lazy:from_list(L))),
			Out=:=L1
		end
	).

prop_drop() ->
	?FORALL(
		{L, N},
		{list(), non_neg_integer()},
		begin
			Out=lazy:to_list(lazy:drop(N, lazy:from_list(L))),
			lists:suffix(Out, L)
		end
	).

prop_dropwhile() ->
	?FORALL(
		{L1, L2},
		{list(), list()},
		begin
			Stop=make_ref(),
			L=L1++[Stop|L2],
			Out=lazy:to_list(lazy:dropwhile(fun (V) -> V=/=Stop end, lazy:from_list(L))),
			Out=:=[Stop|L2]
		end
	).

prop_seq() ->
	?FORALL(
		{N1, N2, S},
		{integer(), integer(), ?SUCHTHAT(S1, integer(), S1=/=0)},
		begin
			Out=lazy:to_list(lazy:seq(N1, N2, S)),
			Exp=if
				S>0, N1>N2 -> [];
				S<0, N1<N2 -> [];
				true -> lists:seq(N1, N2, S)
			end,
			Out=:=Exp
		end
	).

prop_infinite_seq() ->
	?FORALL(
		{N, T, S},
		{integer(), pos_integer(), ?SUCHTHAT(S1, integer(), S1=/=0)},
		begin
			Out1=lazy:to_list(lazy:take(T, lazy:seq(N, infinity, S))),
			Exp1=lists:seq(N, N+(T-1)*S, S),
			Out2=lazy:to_list(lazy:take(T, lazy:seq(N, N, 0))),
			Exp2=[N || _ <- lists:seq(N, N+T-1, 1)],
			Out1=:=Exp1 andalso Out2=:=Exp2
		end
	).

prop_once() ->
	?FORALL(
		V,
		term(),
		begin
			[V]=:=lazy:to_list(lazy:once(V))
		end
	).

prop_repeat() ->
	?FORALL(
		{V, T},
		{term(), pos_integer()},
		begin
			Out=lazy:to_list(lazy:take(T, lazy:repeat(V))),
			Exp=[V ||_ <- lists:seq(1, T)],
			Out=:=Exp
		end
	).

prop_cycle() ->
	?FORALL(
		{L, T},
		{non_empty(list()), pos_integer()},
		begin
			Out=lazy:to_list(lazy:take(T, lazy:cycle(lazy:from_list(L)))),
			Exp=do_cycle_list(T, L),
			Out=:=Exp
		end
	).

do_cycle_list(N, L=[_|_]) ->
	do_cycle_list(N, L, L).

do_cycle_list(0, _, _) ->
	[];
do_cycle_list(N, [], L) ->
	do_cycle_list(N, L, L);
do_cycle_list(N, [E|Es], L) ->
	[E|do_cycle_list(N-1, Es, L)].

prop_filter() ->
	?FORALL(
		L,
		list(oneof([non_neg_integer(), neg_integer()])),
		begin
			FilterFun=fun (V) -> V>=0 end,
			Out=lazy:to_list(lazy:filter(FilterFun, lazy:from_list(L))),
			Exp=lists:filter(FilterFun, L),
			Out=:=Exp
		end
	).

prop_map() ->
	?FORALL(
		L,
		list(),
		begin
			MapFun=fun (V) -> {V} end,
			Out=lazy:to_list(lazy:map(MapFun, lazy:from_list(L))),
			Exp=lists:map(MapFun, L),
			Out=:=Exp
		end
	).

prop_filtermap() ->
	?FORALL(
		L,
		list(oneof([non_neg_integer(), neg_integer(), undefined])),
		begin
			FilterMapFun=fun
				(undefined) -> false;
				(0) -> true;
				(V) when V>0 -> {true, {pos, V}};
				(V) -> {true, {neg, -V}}
			end,
			Out=lazy:to_list(lazy:filtermap(FilterMapFun, lazy:from_list(L))),
			Exp=lists:filtermap(FilterMapFun, L),
			Out=:=Exp
		end
	).

prop_fold() ->
	?FORALL(
		L,
		list(),
		begin
			FoldFun=fun (V, Acc) -> [V|Acc] end,
			Out1=lazy:foldl(FoldFun, [], lazy:from_list(L)),
			Exp1=lists:foldl(FoldFun, [], L),
			Out2=lazy:foldr(FoldFun, [], lazy:from_list(L)),
			Exp2=lists:foldr(FoldFun, [], L),
			Out1=:=Exp1 andalso Out2=:=Exp2
		end
	).

prop_append() ->
	?FORALL(
		Ls,
		list(list()),
		begin
			Out=lazy:to_list(lazy:append([lazy:from_list(L) || L <- Ls])),
			Exp=lists:append(Ls),
			Out=:=Exp
		end
	).

prop_empty() ->
	?FORALL(
		L,
		list(),
		begin
			Out=lazy:to_list(lazy:append([lazy:from_list(L), lazy:empty()])),
			L=:=Out
		end
	).

prop_apply() ->
	?FORALL(
		L,
		list(),
		begin
			Tag=make_ref(),
			Out=lazy:to_list(lazy:apply(fun (V) -> self() ! {Tag, V} end, lazy:from_list(L))),
			RecvdAll=lists:all(
				fun (V) ->
					receive
						{Tag, V1} -> V=:=V1
					after 1000 -> error(timeout)
					end
				end,
				L
			),
			Out=:=L andalso RecvdAll
		end
	).

prop_flush() ->
	?FORALL(
		L,
		list(),
		begin
			Tag=make_ref(),
			ok=lazy:flush(lazy:apply(fun (V) -> self() ! {Tag, V} end, lazy:from_list(L))),
			lists:all(
				fun (V) ->
					receive
						{Tag, V1} -> V=:=V1
					after 1000 -> error(timeout)
					end
				end,
				L
			)
		end
	).

prop_zip() ->
	?FORALL(
		{L1, L2},
		{list(), list()},
		begin
			Out=lazy:to_list(lazy:zip(lazy:from_list(L1), lazy:from_list(L2))),
			MinLen=min(length(L1), length(L2)),
			ExpL1=lists:sublist(L1, MinLen),
			ExpL2=lists:sublist(L2, MinLen),
			Exp=lists:zip(ExpL1, ExpL2),
			Out=:=Exp
		end
	).

prop_zipwith() ->
	?FORALL(
		{L1, L2},
		{list(), list()},
		begin
			ZipFun=fun (V1, V2) when V1=<V2 -> V1; (_, V2) -> V2 end,
			Out=lazy:to_list(lazy:zipwith(ZipFun, lazy:from_list(L1), lazy:from_list(L2))),
			MinLen=min(length(L1), length(L2)),
			ExpL1=lists:sublist(L1, MinLen),
			ExpL2=lists:sublist(L2, MinLen),
			Exp=lists:zipwith(ZipFun, ExpL1, ExpL2),
			Out=:=Exp
		end
	).

prop_all() ->
	?FORALL(
		L,
		list(integer()),
		begin
			Pred=fun (V) -> is_integer(V) end,
			Out1=lazy:all(Pred, lazy:from_list(L)),
			Out2=lazy:all(Pred, lazy:from_list(L++[foo])),
			Out1=:=true andalso Out2=:=false
		end
	).

prop_any() ->
	?FORALL(
		L,
		list(integer()),
		begin
			Pred=fun (V) -> is_atom(V) end,
			Out1=lazy:any(Pred, lazy:from_list(L)),
			Out2=lazy:any(Pred, lazy:from_list(L++[foo])),
			Out1=:=false andalso Out2=:=true
		end
	).

prop_length() ->
	?FORALL(
		L,
		list(),
		begin
			Out=lazy:length(lazy:from_list(L)),
			Exp=length(L),
			Out=:=Exp
		end
	).
