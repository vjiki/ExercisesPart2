%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. апр. 2021 13:15
%%%-------------------------------------------------------------------
-module(lc_db_list).
-author("golubkin").

%% API
-export([start/0,stop/0]).
-export([loop/1]).
-export([insert/2,where_is/1,remove/1,located_at/1]).
-export([all_names/0,all_locations/0]).

start() ->
    register(list_server, spawn(fun() -> loop([]) end)),
    ok.

stop() ->
    list_server ! stop,
    server_terminated.

loop(Db) ->
    receive
        {From, insert, Name, Location} ->
            %%io:format("in the DB ~p ~n", [Db]),
            case (lists:member({Name,Location}, Db)) of
                true  ->  From ! already_inserted,
                    loop(Db);
                false ->  Db1 =  lists:append(Db, [{Name, Location}]),
                    From ! ok,
                    loop(Db1)
            end;
        {From, where_is, Name} ->
            case [Value || {ListKey, Value} <- Db, ListKey == Name] of
                [] ->  From ! no_such_name;
                Locations  -> From ! Locations
            end,
            loop(Db);
        {From, remove, Name} ->
            Db1 = [{ListKey, Value} || {ListKey, Value} <- Db, Name /= ListKey],
            From ! ok,
            loop(Db1);
        {From, located_at, Location} ->
            case [Name || {Name, Loc} <- Db, Loc == Location] of
                [] ->
                    From ! none;
                Names ->
                    From ! Names
            end,
            loop(Db);
        {From, fetch_all_names} ->
            SortedDb = lists:keysort(1,Db),
            Names = [Name || {Name, _} <- SortedDb],
            From ! Names,
            loop(Db);
        {From, fetch_all_locations} ->
            %%SortedDb = lists:keysort(2,Db),
            Locations = [Loc || {_, Loc} <- Db],
            SortedLocations = lists:usort(Locations),
            %%L = proplists:get(SortedDb1),
            From ! SortedLocations,
            loop(Db);
        stop ->
            io:format("stopping PID ~w ~n", [self()]),
            true;
        _Other ->
            io:format("Other message: ~p~n", [_Other]),
            loop(Db)
    end.

insert(Name, Location) ->
    %%io:format("~p from PID ~w ~n", [?FUNCTION_NAME, self()]),
    list_server ! {self(), insert, Name, Location},
    receive
        ok -> ok;
        already_inserted -> already_inserted
    end.

where_is(Name) ->
    %%io:format("~p from PID ~w ~n", [?FUNCTION_NAME, self()]),
    list_server ! {self(), where_is, Name},
    receive
        no_such_name -> no_such_name;
        Location -> Location
    end.

remove(Name) ->
    list_server ! {self(), remove, Name},
    receive
        ok -> ok
    end.

located_at(Location) ->
    list_server ! {self(), located_at, Location},
    receive
        none -> none;
        Names -> Names
    end.

all_names() ->
    list_server ! {self(), fetch_all_names},
    receive
        none -> [];
        Names -> Names
    end.

all_locations() ->
    list_server ! {self(), fetch_all_locations},
    receive
        none -> [];
        Names -> Names
    end.

