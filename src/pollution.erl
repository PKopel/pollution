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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3,
  getMinTypeMean/2, getTwoClosestStations/1]).

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
    Other -> Other
  end.

typeMean(Type) -> fun([S | _]) ->
  case lists:foldl(fun({_, T, V}, {Sum, N}) ->
    if T == Type -> {Sum + V, N + 1};
      true -> {Sum, N}
    end
                   end, {0, 0}, S#station.measurements) of
    {0, 0} -> {error, no_such_record};
    {Value, Number} -> Value / Number
  end;
  ([]) -> {error, no_such_station} end.

getStationMean(Monitor, Station, Type) ->
  searchMonitor(Monitor, Station, typeMean(Type)).

getMinTypeMean(Monitor, Type) ->
  TypeMean = typeMean(Type),
  MinMean = fun(S, {MinS, MinMean}) ->
    Mean = TypeMean([S]),
    if MinMean == 0; MinMean > Mean -> {S, Mean};
      true -> {MinS, MinMean}
    end;
    (empty, Acc) -> Acc;
    (_, _) -> {error, wrong_arguments} end,
  {S, Mean} = lists:foldl(MinMean, {empty, 0}, Monitor),
  {S#station.name, Mean}.

distanceFrom(#station{coords = {X1, Y1}}) -> fun(#station{coords = {X2, Y2}}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2));
  (_) -> {error, wrong_arguments} end.

closestTwo([_], {A, B, MinDist}) ->
  {A, B, MinDist};
closestTwo([S | T], {A, B, MinDist}) ->
  DistanceFromS = distanceFrom(S),
  [Closest | _] = lists:sort(fun(X, Y) -> DistanceFromS(X) > DistanceFromS(Y) end, T),
  Distance = DistanceFromS(Closest),
  if MinDist == 0; MinDist > Distance -> closestTwo(T, {S, Closest, Distance});
    true -> closestTwo(T, {A, B, MinDist})
  end;
closestTwo(_, _) ->
  {error, wrong_arguments}.

getTwoClosestStations(Monitor) ->
  {S1, S2, Dist} = closestTwo(Monitor, {empty, empty, 0}),
  {S1#station.name, S2#station.name, Dist}.
