%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. апр. 2021 15:34
%%%-------------------------------------------------------------------
-module(db_ets).
-author("golubkin").
-include_lib("db_record.hrl").

%% API
-export([start/0,stop/0]).
-export([loop/1]).
-export([insert/3,where_is/1,remove/1,located_at/1]).
-export([working_at/1]).
-export([all_names/0,all_locations/0]).

start() ->
    register(list_server, spawn(fun() -> loop(ets:new(my_ets, [bag,protected,{keypos,#person.name}])) end)),
    ok.

stop() ->
    list_server ! stop,
    server_terminated.

loop(Db) ->
    receive
        {From, insert, Name, Location, Company} ->
            %%io:format("in the DB ~p ~n", [Db]),
            NewPerson = #person{name=Name, location=Location, company=Company},
            case (ets:member(Db, NewPerson)) of
                true  ->  From ! already_inserted,
                    loop(Db);
                false ->  ets:insert(Db, NewPerson),
                    From ! ok,
                    loop(Db)
            end;
        {From, find, Name} ->
            Locations = lists:append(ets:match(Db, #person{name=Name, location = '$2', company ='_'})),
            case Locations == [] of
                false ->
                    From ! Locations;
                true ->
                    From ! no_such_name
            end,
            loop(Db);
        {From, remove, Name} ->
            ets:delete(Db, Name),
            From ! ok,
            loop(Db);
        {From, find_all, Location} ->
            Names = lists:append(ets:match(Db, #person{name='$1', location = Location, company ='_'})),
            case Names == [] of
                false ->
                    From ! Names;
                true ->
                    From ! none
            end,
            loop(Db);
        {From, fetch_all_names} ->
            M = lists:merge(ets:match(Db, #person{name='$1', location ='_', company ='_'})),
            From ! M,
            loop(Db);
        {From, fetch_all_locations} ->
            M = lists:usort(lists:append(ets:match(Db, #person{name='_', location ='$2', company ='_'}))),
            From ! M,
            loop(Db);
        {From, find_all_names_in_company, Company} ->
            Names = lists:append(ets:match(Db, #person{name='$1', location = '_', company =Company})),
            case Names == [] of
                false ->
                    From ! Names;
                true ->
                    From ! none
            end,
            loop(Db);
        stop ->
            io:format("stopping PID ~w ~n", [self()]),
            true;
        _Other ->
            io:format("Other message: ~p~n", [_Other]),
            loop(Db)
    end.

insert(Name, Location, Company) ->
    %%io:format("~p from PID ~w ~n", [?FUNCTION_NAME, self()]),
    list_server ! {self(), insert, Name, Location, Company},
    receive
        ok -> ok;
        already_inserted -> already_inserted
    end.

where_is(Name) ->
    %%io:format("~p from PID ~w ~n", [?FUNCTION_NAME, self()]),
    list_server ! {self(), find, Name},
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
    list_server ! {self(), find_all, Location},
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


working_at(Company) ->
    list_server ! {self(), find_all_names_in_company, Company},
    receive
        none -> none;
        Names -> Names
    end.
