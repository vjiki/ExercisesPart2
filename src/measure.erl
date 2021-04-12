%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. апр. 2021 13:39
%%%-------------------------------------------------------------------
-module(measure).
-author("golubkin").

%% API
-export([gen/2,time_insert_db/2,time/3]).

gen(_Module, 0) ->
    ok;
gen(Module, N) ->
    Module:insert({name, N}, getLocation(), getCompany()),
    gen(Module, N-1).

getLocation() ->
    lists:nth(rand:uniform(3), [home, work, school]).

getCompany() ->
    lists:nth(rand:uniform(3), [ericsson, nokia, motorola]).

time_insert_db(Module, N) ->
    {MicroSec, _} = timer:tc(?MODULE, gen, [Module, N]),
    io:format("~s  number: ~p insert=~p micro seconds~n",[Module, N, MicroSec]).

time(M,F,A) ->
    {MicroSec, R} = timer:tc(M, F, A),
    io:format("~s ~s ~p call=~p micro seconds return ~p ~n",[M, F, A, MicroSec, R]).