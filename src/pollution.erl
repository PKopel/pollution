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
-record(station, {name, coords, measurements = []}).

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).

searchMonitor([Current | T], {X, Y}, Func) ->
  if Current#station.coords == {X, Y} -> Func([Current | T]);
    true -> Result = searchMonitor(T, {X, Y}, Func),
      if is_list(Result) -> [Current | Result];
        true -> Result
      end
  end;
searchMonitor([Current | T], Name, Func) ->
  if Current#station.name == Name -> Func([Current | T]);
    true -> Result = searchMonitor(T, Name, Func),
      if is_list(Result) -> [Current | Result];
        true -> Result
      end
  end;
searchMonitor(Monitor, _, Func) ->
  Func(Monitor).

createMonitor() ->
  [].

addStation(Monitor, Name, Coords) ->
  AddStation = fun([]) -> [#station{name = Name, coords = Coords}];
    ([S | _]) ->
      if S#station.name == Name -> {error, name_already_used};
        S#station.coords == Coords -> {error, coords_already_used};
        true -> {error, wrong_arguments}
      end;
    (_) -> {error, wrong_arguments} end,
  searchMonitor(Monitor, Coords, AddStation).

filter(Date, Type) -> fun({D, T, _}) when D == Date, T == Type -> true;(_) -> false end.

addValue(Monitor, Station, Date, Type, Value) ->
  AddValue = fun([S | T]) ->
    case lists:any(filter(Date, Type), S#station.measurements) of
      true -> {error, measurement_already_recorded};
      false -> OldList = S#station.measurements,
        [S#station{measurements = [{Date, Type, Value} | OldList]} | T]
    end;
    ([]) -> {error, no_such_station} end,
  searchMonitor(Monitor, Station, AddValue).

removeValue(Monitor, Station, Date, Type) ->
  Filter = filter(Date, Type),
  RemoveValue = fun([S | T]) ->
    case lists:any(Filter, S#station.measurements) of
      true -> OldList = S#station.measurements,
        [S#station{measurements = lists:filter(fun(R) -> not Filter(R) end, OldList)} | T];
      false -> {error, no_such_record}
    end;
    ([]) -> {error, no_such_station} end,
  searchMonitor(Monitor, Station, RemoveValue).

getOneValue(Monitor, Station, Date, Type) ->
  GetValue = fun([S | _]) ->
    case lists:filter(filter(Date, Type), S#station.measurements) of
      [{Date, Type, Value}] -> Value;
      [] -> {error, no_such_measurement}
    end;
    ([]) -> {error, no_such_station}
             end,
  searchMonitor(Monitor, Station, GetValue).

getStationMean(Monitor, Station, Type) ->
  TypeMean = fun([S | _]) ->
    case lists:foldl(fun({_, T, V}, {Sum, N}) ->
      if T == Type -> {Sum + V, N + 1};
        true -> {Sum, N}
      end
                     end, {0, 0}, S#station.measurements) of
      {0, 0} -> {error, no_such_record};
      {Value, Number} -> Value / Number
    end;
    ([]) -> {error, no_such_station} end,
  searchMonitor(Monitor, Station, TypeMean).

getDailyMean(Monitor, Type, {Year, Month, Day}) ->
  DailyMean = fun(S, {SumAcc, NumAcc}) ->
    {Value, Number} = lists:foldl(fun({{{Y, M, D}, _}, T, V}, {Sum, N}) ->
      if T == Type, Y == Year, M == Month, D == Day -> {Sum + V, N + 1};
        true -> {Sum, N}
      end
                                  end, {0, 0}, S#station.measurements),
    {SumAcc + Value, NumAcc + Number};
    (empty, Acc) -> Acc;
    (_, _) -> {error, wrong_arguments} end,
  case lists:foldl(DailyMean, {0, 0}, Monitor) of
    {0, 0} -> 0;
    {Sum, Number} -> Sum / Number;
    _ -> {error, wrong_arguments}
  end.