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

newStation(Name, Coords) ->
  {{Name, Coords, []}, {empty, empty}, {empty, empty}}.

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
  end;
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
searchMonitor(Monitor, _, Func) ->
  Func(Monitor).

walkMonitor({Current, {Smaller, Larger}, _}, Func, Acc) ->
  SmallerAcc = walkMonitor(Smaller, Func, Acc),
  LargerAcc = walkMonitor(Larger, Func, SmallerAcc),
  Func(Current, LargerAcc);
walkMonitor(Monitor, Func, Acc) ->
  Func(Monitor, Acc).



createMonitor() ->
  empty.

addStation(empty, Name, Coords) ->
  newStation(Name, Coords);
addStation(Monitor, Name, Coords) ->
  NewStation = newStation(Name, Coords),
  AddStation = fun(empty) -> NewStation;
    ({N, C, _}) ->
      if N == Name -> {error, name_already_used};
        C == Coords -> {error, coords_already_used};
        true -> {error, wrong_arguments}
      end;
    (_) -> {error, wrong_arguments} end,
  NewMonitor = searchMonitor(Monitor, Name, AddStation),
  searchMonitor(NewMonitor, Coords, AddStation).

addValue(Monitor, Station, Date, Type, Value) ->
  AddValue = fun({{N, Coords, List}, NameChildren, CoordsChildren}) ->
    case lists:any(fun({D, T, _}) when D == Date, T == Type -> true;
      (_) -> false
                   end, List) of
      true -> {error, measurement_already_recorded};
      false -> {{N, Coords, [{Date, Type, Value} | List]}, NameChildren, CoordsChildren}
    end;
    (_) -> {error, no_such_station} end,
  searchMonitor(Monitor, Station, AddValue).

removeValue(Monitor, Station, Date, Type) ->
  RemoveValue = fun({{N, Coords, List}, NameChildren, CoordsChildren}) ->
    Filter = fun({D, T, _}) when D == Date, T == Type -> true;
      (_) -> false
             end,
    case lists:any(Filter, List) of
      true -> {{N, Coords, lists:filter(fun(R) -> not Filter(R) end, List)}, NameChildren, CoordsChildren};
      false -> {error, no_such_record}
    end;
    (_) -> {error, no_such_station} end,
  searchMonitor(Monitor, Station, RemoveValue).

getOneValue(Monitor, Station, Date, Type) ->
  SearchStation = fun({{N, C, List}, _, _}) when C == Station; N == Station -> List;
    (_) -> {error, no_such_station}
                  end,
  List = searchMonitor(Monitor, Station, SearchStation),
  case lists:filter(fun({D, T, _}) when D == Date, T == Type -> true; (_) -> false end, List) of
    [{Date, Type, Value}] -> Value;
    [] -> {error, no_such_measurement}
  end.

getStationMean(Monitor, Station, Type) ->
  TypeMean = fun({{_, _, List}, _, _}) ->
    case lists:foldl(fun({_, T, V}, {Sum, N}) ->
      if T == Type -> {Sum + V, N + 1};
        true -> {Sum, N}
      end
                     end, {0, 0}, List) of
      {0, 0} -> {error, no_such_record};
      {Value, Number} -> Value / Number
    end;
    (_) -> {error, no_such_station} end,
  searchMonitor(Monitor, Station, TypeMean).

getDailyMean(Monitor, Type, {Year, Month, Day}) ->
  DailyMean = fun({_, _, List}, {SumAcc, NumAcc}) ->
    {Value, Number} = lists:foldl(fun({{{Y, M, D}, _}, T, V}, {Sum, N}) ->
      if T == Type, Y == Year, M == Month, D == Day -> {Sum + V, N + 1};
        true -> {Sum, N}
      end
                                  end, {0, 0}, List),
    {SumAcc + Value, NumAcc + Number};
    (empty, Acc) -> Acc;
    (_, _) -> {error, wrong_arguments} end,
  case walkMonitor(Monitor, DailyMean, {0, 0}) of
    {0, 0} -> 0;
    {Sum, Number} -> Sum / Number;
    _ -> {error, wrong_arguments}
  end.