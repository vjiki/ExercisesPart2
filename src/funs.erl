%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. апр. 2021 16:12
%%%-------------------------------------------------------------------
-module(funs).
-author("golubkin").

%% API
-export([double/1]).
-export([llen/1,min/1]).
-export([all/2, all_even/1, first/2]).
-export([mk_adder/1]).

double(X) -> 2*X.

llen(L) -> lists:map(fun(X)-> length(X) end, L).

min(L) -> lists:foldl(fun(X, Y) -> min(X,Y) end, lists:last(L), L).

all(Pred, List) ->
    lists:foldl(fun(X, Y) ->
        case Y of
            true  -> Pred(X);
            false -> false
        end
                end, true, List).

all_even(List) ->
    all(fun(X) -> (X rem 2) =:= 0  end, List).

first(Pred, List) ->
    lists:foldl(fun(X, Y) ->
        case Y of
            false -> case Pred(X) of
                         true -> X;
                         false -> false
                     end;
            M  -> M
        end
                end, false, List).

mk_adder(Const) -> fun (X) -> Const + X end.