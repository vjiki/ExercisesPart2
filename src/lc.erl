%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. апр. 2021 12:37
%%%-------------------------------------------------------------------
-module(lc).
-author("golubkin").

%% API
-export([atoms/1,atoms_ints/1]).
-export([keysearch/2,join/2,diff/2]).

%%lc:atoms([{hello, 1}, 3, ernie, [a, b, c], burt]).
%%[ernie, burt]
atoms(List) -> [Elem || Elem <- List, is_atom(Elem)].
    %[Name || #person{name=Name, location=Loc} <- Db, Loc == Location].

%%2> lc:atoms_ints([{hello, 1}, 3, ernie, [a, b, c], burt]).
%%[3,ernie,burt]
atoms_ints(List) -> [Elem || Elem <- List, is_atom(Elem) or is_integer(Elem) ].

%%lc:keysearch(b, [{a, 1},{b, 2},{c, 3}]).
%%[{b,2}]
keysearch(Key, List) -> [{ListKey, Value} || {ListKey, Value} <- List, Key == ListKey].

%%3>	lc:join([1, 2, 3, 4, 5, 6], [2, 4, 6, 8, 10, 12]) .
%%[2,4,6]
join(List1, List2) -> [Elem || Elem <- List1, lists:member(Elem, List2)].

%%4>	lc:diff([1, 2, 3, 4, 5, 6], [2, 4, 6, 8, 10,12]).
%%[1,3,5]

diff(List1, List2) -> [Elem || Elem <- List1, not lists:member(Elem, List2)].
