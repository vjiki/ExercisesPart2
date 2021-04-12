%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. апр. 2021 12:46
%%%-------------------------------------------------------------------
-module(db_record).
-author("golubkin").
-include_lib("db_record.hrl").

%% API
-export([start/0,stop/0]).
-export([loop/1]).
-export([insert/3,where_is/1,remove/1,located_at/1]).
-export([working_at/1]).
-export([all_names/0,all_locations/0]).

start() ->
    register(list_server, spawn(fun() -> loop([]) end)),
    ok.

stop() ->
    list_server ! stop,
    server_terminated.

loop(Db) ->
    receive
        {From, insert, Name, Location, Company} ->
            %%io:format("in the DB ~p ~n", [Db]),
            NewPerson = #person{name=Name, location=Location, company=Company},
            case (lists:member(NewPerson, Db)) of
                true  ->  From ! already_inserted,
                          loop(Db);
                false ->  Db1 =  lists:append(Db, [NewPerson]),
                          From ! ok,
                          loop(Db1)
            end;
        {From, find, Name} ->
            case lists:keyfind(Name, #person.name, Db) of
                #person{location = Location}  -> From ! Location;
                false ->  From ! no_such_name
            end,
                loop(Db);
        {From, remove, Name} ->
              Db1 = lists:keydelete(Name,#person.name,Db),
              From ! ok,
              loop(Db1);
        {From, find_all, Location} ->
            Names = [Name || #person{name=Name, location=Loc} <- Db, Loc == Location],
            case Names == [] of
                false ->
                    From ! Names;
                true ->
                    From ! none
            end,
            loop(Db);
        {From, fetch_all_names} ->
            SortedDb = lists:keysort(#person.name,Db),
            Names = [Name || #person{name=Name} <- SortedDb],
            From ! Names,
            loop(Db);
        {From, fetch_all_locations} ->
            %%SortedDb = lists:keysort(2,Db),
            Locations = [Loc || #person{location=Loc} <- Db],
            SortedLocations = lists:usort(Locations),
            %%L = proplists:get(SortedDb1),
            From ! SortedLocations,
            loop(Db);
        {From, find_all_names_in_company, Company} ->
            Names = [Name || #person{name=Name, company=Comp} <- Db, Comp == Company],
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
