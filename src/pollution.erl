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

%% @hidden Searching station in monitor:
%% by coordinates
searchMonitor([Current | T], {X, Y}, Func) ->
  if Current#station.coords == {X, Y} -> Func([Current | T]);
    true -> Result = searchMonitor(T, {X, Y}, Func),
      if is_list(Result) -> [Current | Result];
        true -> Result
      end
  end;
%% by name
searchMonitor([Current | T], Name, Func) ->
  if Current#station.name == Name -> Func([Current | T]);
    true -> Result = searchMonitor(T, Name, Func),
      if is_list(Result) -> [Current | Result];
        true -> Result
      end
  end;
%% edge condition
searchMonitor(Monitor, _, Func) ->
  Func(Monitor).

%% @doc Create monitor:
%% Returns new empty monitor
%% @end
createMonitor() ->
  case application:get_env(monitor) of
    {ok, empty} -> [];
    {ok, Monitor} -> Monitor
  end.

%% @doc Add new station: check if station is already in monitor,
%% if not put new one at the beginning.
%% Returns new monitor or {error, station_already_exists} if
%% monitor already contains station with given Name or coordinates.
%% @end
addStation(Monitor, Name, {X, Y}) ->
  CompareStation = fun(#station{name = N, coords = C}) ->
    if N == Name -> true;
      C == {X, Y} -> true;
      true -> false
    end;
    (_) -> false end,
  case lists:any(CompareStation, Monitor) of
    true -> {error, station_already_exists};
    false -> [#station{name = Name, coords = {X, Y}} | Monitor]
  end.

%% @hidden helper function for searching measurement list
filter(Date, Type) -> fun({D, T, _}) when D == Date, T == Type -> true;(_) -> false end.

%% @doc Add measurement: get station from monitor,
%% check if measurement is already recorded in that station,
%% if not put it at the beginning.
%% Returns {error, measurement_already_recorded} if given station
%% contains measurement of Type form Date, {error, no_such_station}
%% if monitor doesn't contain station with name or coordinates equal
%% to Station or new monitor with new measurement recorded.
%% @end
addValue(Monitor, Station, Date, Type, Value) ->
  AddValue = fun([S | T]) ->
    case lists:any(filter(Date, Type), S#station.measurements) of
      true -> {error, measurement_already_recorded};
      false -> OldList = S#station.measurements,
        [S#station{measurements = [{Date, Type, Value} | OldList]} | T]
    end;
    ([]) -> {error, no_such_station} end,
  searchMonitor(Monitor, Station, AddValue).

%% @doc Remove measurement: get station from monitor,
%% check if measurement is recorded in that station
%% and remove it.
%% Returns {error, no_such_measurement} if given station doesn't
%% contains measurement of Type form Date, {error, no_such_station}
%% if monitor doesn't contain station with name or coordinates equal
%% to Station or new monitor without specified measurement.
%% @end
removeValue(Monitor, Station, Date, Type) ->
  Filter = filter(Date, Type),
  RemoveValue = fun([S | T]) ->
    case lists:any(Filter, S#station.measurements) of
      true -> OldList = S#station.measurements,
        [S#station{measurements = lists:filter(fun(R) -> not Filter(R) end, OldList)} | T];
      false -> {error, no_such_measurement}
    end;
    ([]) -> {error, no_such_station} end,
  searchMonitor(Monitor, Station, RemoveValue).

%% @doc Get measurement: get station from monitor,
%% check if measurement is recorded in that station
%% and return it.
%% Returns {error, no_such_measurement} if given station doesn't
%% contains measurement of Type form Date, {error, no_such_station}
%% if monitor doesn't contain station with name or coordinates equal
%% to Station or value of requested measurement.
%% @end
getOneValue(Monitor, Station, Date, Type) ->
  GetValue = fun([S | _]) ->
    case lists:filter(filter(Date, Type), S#station.measurements) of
      [{Date, Type, Value}] -> Value;
      [] -> {error, no_such_measurement}
    end;
    ([]) -> {error, no_such_station}
             end,
  searchMonitor(Monitor, Station, GetValue).

%% @doc Dail mean: get mean value of measurements of Type
%% from {Year, Month, Day}.
%% Returns mean value of measurements or 0 if there are no
%% measurements of Type from {Year, Month, Day} in Monitor
%% @end
getDailyMean(Monitor, Type, {Year, Month, Day}) ->
  DailyMean = fun(S, {SumAcc, NumAcc}) ->
    {Value, Number} = lists:foldl(
      fun({{{Y, M, D}, _}, T, V}, {Sum, N}) ->
        if T == Type, Y == Year, M == Month, D == Day -> {Sum + V, N + 1};
          true -> {Sum, N}
        end
      end, {0, 0}, S#station.measurements
    ),
    {SumAcc + Value, NumAcc + Number};
    (empty, Acc) -> Acc end,
  case lists:foldl(DailyMean, {0, 0}, Monitor) of
    {0, 0} -> 0;
    {Sum, Number} -> Sum / Number
  end.

%% @hidden helper function for computing mean value of measurements of Type in one station
typeMean(Type) -> fun([S | _]) ->
  case lists:foldl(
    fun({_, T, V}, {Sum, N}) ->
      if T == Type -> {Sum + V, N + 1};
        true -> {Sum, N}
      end
    end, {0, 0}, S#station.measurements
  ) of
    {0, 0} -> 0;
    {Value, Number} -> Value / Number
  end;
  ([]) -> {error, no_such_station} end.

%% @doc Mean value of Type in Station: get Station from monitor
%% and compute mean value of measurements of Type in it.
%% Returns {error, no_such_station} if monitor doesn't contain
%% station with name or coordinates equal to Station, mean
%% value of measurements of Type or 0 in case there are no such
%% measurements.
%% @end
getStationMean(Monitor, Station, Type) ->
  searchMonitor(Monitor, Station, typeMean(Type)).

%% @doc Station with minimal mean value of Type: find station in
%% Monitor with loves mean value of measurements of Type.
%% Returns {empty, 0} if there are no measurements of Type in
%% Monitor or {Station_name, Mean_value}.
%% @end
getMinTypeMean(Monitor, Type) ->
  TypeMean = typeMean(Type),
  MinMean = fun(S, {MinS, MinMean}) ->
    case TypeMean([S]) of
      {error, Reason} -> {error, Reason};
      Mean ->
        if MinMean == 0; MinMean > Mean -> {S, Mean};
          true -> {MinS, MinMean}
        end
    end;
    (empty, Acc) -> Acc;
    (_, _) -> {error, wrong_arguments} end,
  case lists:foldl(MinMean, {empty, 0}, Monitor) of
    {S, Mean} -> {S#station.name, Mean};
    Other -> Other
  end.

%% @hidden helper function for computing distance between stations
distanceFrom(#station{coords = {X1, Y1}}) -> fun(#station{coords = {X2, Y2}}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2));
  (_) -> {error, wrong_arguments} end.

%% @hidden helper function for finding two closest stations:
%% edge condition
closestTwo([], {A, B, MinDist}) ->
  {A, B, MinDist};
%% for station S: computes distance do all stations behind S in monitor,
%% (stations before S where checked earlier), if there is a station Closest
%% closer to S than current minimal distance replaces current pair
%% with S and Closest
closestTwo([S | T], {A, B, MinDist}) ->
  DistanceFromS = distanceFrom(S),
  [Closest | _] = lists:sort(fun(X, Y) -> DistanceFromS(X) < DistanceFromS(Y) end, T),
  Distance = DistanceFromS(Closest),
  if MinDist > Distance -> closestTwo(T, {S, Closest, Distance});
    true -> closestTwo(T, {A, B, MinDist})
  end;
closestTwo(_, _) ->
  {error, wrong_arguments}.

%% @doc Two closest stations: get two stations in Monitor
%% that are closest to each other.
%% Returns {empty, empty, infinity} if monitor contains no more than one station,
%% {Station_A_name, Station_B_name, distance} otherwise.
%% @end
getTwoClosestStations(Monitor) ->
  case closestTwo(Monitor, {empty, empty, infinity}) of
    {S1 = #station{}, S2 = #station{}, Dist}  -> {S1#station.name, S2#station.name, Dist};
    Other -> Other
  end.
