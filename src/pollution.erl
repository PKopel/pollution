%%%-------------------------------------------------------------------
%%% @author pkopel
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Mar 2020 14:53
%%%-------------------------------------------------------------------
-module(pollution).
-author("pkopel").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).

getName({Name, _, _}) ->
  Name.

getCoords({_, Coords, _}) ->
  Coords.

emptyStation() ->
  {empty}.

newStation(Name, Coords) ->
  {{Name, Coords, []}, {emptyStation(), emptyStation()}, {emptyStation(), emptyStation()}}.

searchMonitor({empty}, _, Func) ->
  Func(empty);
searchMonitor({Current, {Smaller, Larger}, CoordsChildren}, Name, Func) ->
  CurrentName = getName(Current),
  if CurrentName > Name ->
    case searchMonitor(Smaller, Name, Func) of
      {D, NC, CC} -> {Current, {{D, NC, CC}, Larger}, CoordsChildren};
      Other -> Other
    end;
    CurrentName < Name ->
      case searchMonitor(Larger, Name, Func) of
        {D, NC, CC} -> {Current, {Smaller, {D, NC, CC}}, CoordsChildren};
        Other -> Other
      end;
    true -> Func({Current, {Smaller, Larger}, CoordsChildren})
  end;
searchMonitor({Current, NameChildren, {Smaller, Larger}}, {X, Y}, Func) ->
  CurrentCoords = getCoords(Current),
  if CurrentCoords > {X, Y} ->
    case searchMonitor(Smaller, {X, Y}, Func) of
      {D, NC, CC} -> {Current, NameChildren, {{D, NC, CC}, Larger}};
      Other -> Other
    end;
    CurrentCoords < {X, Y} ->
      case searchMonitor(Larger, {X, Y}, Func) of
        {D, NC, CC} -> {Current, NameChildren, {Smaller, {D, NC, CC}}};
        Other -> Other
      end;
    true -> Func({Current, NameChildren, {Smaller, Larger}})
  end.

createMonitor() ->
  emptyStation().

addStation({empty}, Name, Coords) ->
  newStation(Name, Coords);
addStation(Monitor, Name, Coords) ->
  NewStation = newStation(Name, Coords),
  AddStation = fun Add(empty) -> NewStation;
    Add({N, C, _}) ->
      if N == Name -> {error, name_already_used};
        C == Coords -> {error, coords_already_used};
        true -> {error, wrong_arguments}
      end end,
  NewMonitor = searchMonitor(Monitor, Name, AddStation),
  searchMonitor(NewMonitor, Coords, AddStation).

addValue(Monitor, Station, Date, Type, Value) ->
  AddValue = fun Add(empty) -> {error, no_such_station};
    Add({{N, Coords, List}, NameChildren, CoordsChildren}) ->
      case lists:any(fun({D, T, _}) -> D == Date and T == Type end, List) of
        true -> {error, measurement_already_recorded};
        false -> {{N, Coords, [{Date, Type, Value} | List]}, NameChildren, CoordsChildren}
      end end,
  searchMonitor(Monitor, Station, AddValue).

removeValue(Monitor, Station, Date, Type) ->
  RemoveValue = fun Remove(empty) -> {error, no_such_station};
    Remove({{N, Coords, List}, NameChildren, CoordsChildren}) ->
      Filter = fun({D, T, _}) -> D == Date and T == Type end,
      case lists:any(Filter, List) of
        true -> {{N, Coords, lists:filter(fun(R) -> not Filter(R) end, List)}, NameChildren, CoordsChildren};
        false -> {error, no_such_record}
      end end,
  searchMonitor(Monitor, Station, RemoveValue).

getOneValue(Monitor, Station, Date, Type) ->
  SearchStation = fun Search(empty) -> {error, no_such_station};
    Search({{N, C, List}, NameChildren, CoordsChildren}) ->
      if C == Station or N == Station -> List;
        true -> {error, no_such_station}
      end end,
  List = searchMonitor(Monitor, Station, SearchStation),
  [{Date, Type, Value}] = lists:filter(fun({D, T, _}) -> D == Date and T == Type end, List),
  Value.

getStationMean(Monitor, Station, Type) ->
.

getDailyMean(Monitor, Type, Date) ->
.