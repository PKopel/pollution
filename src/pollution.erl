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

%wyszukiwanie stacji w monitorze:
% po współrzędnych
searchMonitor([Current | T], {X, Y}, Func) ->
  if Current#station.coords == {X, Y} -> Func([Current | T]);
    true -> Result = searchMonitor(T, {X, Y}, Func),
      if is_list(Result) -> [Current | Result];
        true -> Result
      end
  end;
% po nazwie
searchMonitor([Current | T], Name, Func) ->
  if Current#station.name == Name -> Func([Current | T]);
    true -> Result = searchMonitor(T, Name, Func),
      if is_list(Result) -> [Current | Result];
        true -> Result
      end
  end;
% przypadek brzegowy
searchMonitor(Monitor, _, Func) ->
  Func(Monitor).

createMonitor() ->
  [].

%dodanie nowej stacji: przeszukuje monitor pod kątem istnienia dodawanej stacji,
% jeśli jej jeszcze nie ma to umieszcza ją na początku
addStation(Monitor, Name, {X, Y}) ->
  CompareStation = fun(S) ->
      if S#station.name == Name -> true;
        S#station.coords == {X, Y} -> true;
        true -> false
      end;
    (_) -> false end,
  case lists:any( CompareStation, Monitor) of
    true -> {error, station_already_exists};
    false -> [#station{name = Name, coords = {X, Y}} | Monitor]
  end.

%funkcja pomocnicza do przeszukiwania listy pomiarów
filter(Date, Type) -> fun({D, T, _}) when D == Date, T == Type -> true;(_) -> false end.

%dodanie pomiaru: przeszukuje monitor aż znajdzie właściwą stację,
% następnie sprawdza czy pomiar nie został już zapisany
% i jesli nie, to umieszcza go na początku listy
addValue(Monitor, Station, Date, Type, Value) ->
  AddValue = fun([S | T]) ->
    case lists:any(filter(Date, Type), S#station.measurements) of
      true -> {error, measurement_already_recorded};
      false -> OldList = S#station.measurements,
        [S#station{measurements = [{Date, Type, Value} | OldList]} | T]
    end;
    ([]) -> {error, no_such_station} end,
  searchMonitor(Monitor, Station, AddValue).

%usuwanie pomiaru: przeszukuje monitor aż znajdzie właściwą stację,
% następnie sprawdza czy dany pomiar istnieje i usuwa go
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

%usuwanie pomiaru: przeszukuje monitor aż znajdzie właściwą stację,
% następnie sprawdza czy dany pomiar istnieje i zwraca jego wartość
getOneValue(Monitor, Station, Date, Type) ->
  GetValue = fun([S | _]) ->
    case lists:filter(filter(Date, Type), S#station.measurements) of
      [{Date, Type, Value}] -> Value;
      [] -> {error, no_such_measurement}
    end;
    ([]) -> {error, no_such_station}
             end,
  searchMonitor(Monitor, Station, GetValue).

%średnia dzienna: korzystając z funkcji foldl na monitorze oblicza sumę wartości
% i ilość  pomiarów danego typu danego dnia, zwraca ich iloraz
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

%funkcja pomocnicza do obliczania średniej wartośc danego typu pomiarów w danej stacji
typeMean(Type) -> fun([S | _]) ->
  case lists:foldl(fun({_, T, V}, {Sum, N}) ->
    if T == Type -> {Sum + V, N + 1};
      true -> {Sum, N}
    end
                   end, {0, 0}, S#station.measurements) of
    {0, 0} -> {error, no_such_measurement};
    {Value, Number} -> Value / Number
  end;
  ([]) -> {error, no_such_station} end.

%średnia danego typu pomiarów w danej stacji: przeszukuje monitor
% i jeśli znajdzie właściwą stację to liczy i zwraca średnią korzystając z poprzedniej funkcji
getStationMean(Monitor, Station, Type) ->
  searchMonitor(Monitor, Station, typeMean(Type)).

%stacja o najmniejszej średniej danego typu pomiarów: korzystając z foldl na monitorze i funkcji
% typeMean znajduje odpowiednią stację, zwraca {nazwa stacji, wartość średniej}
getMinTypeMean(Monitor, Type) ->
  TypeMean = typeMean(Type),
  MinMean = fun(S, {MinS, MinMean}) ->
    Mean = TypeMean([S]),
    if MinMean == 0; MinMean > Mean -> {S, Mean};
      true -> {MinS, MinMean}
    end;
    (empty, Acc) -> Acc;
    (_, _) -> {error, wrong_arguments} end,
  case lists:foldl(MinMean, {empty, 0}, Monitor) of
    {S, Mean} -> {S#station.name, Mean};
    Other -> Other
  end.

%funkcja pomocnicza do obliczania odległości między stacjami
distanceFrom(#station{coords = {X1, Y1}}) -> fun(#station{coords = {X2, Y2}}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2));
  (_) -> {error, wrong_arguments} end.

%funkcja pomocnicza do znajdywania dwóch stacji leżących najbliżej siebie:
%przypadek brzegowy
closestTwo([_], {A, B, MinDist}) ->
  {A, B, MinDist};
%dla danej stacji w monitorze: oblicza odległość do wszystkich stacj znajdujących się
% w ogonie monitora (wcześniejsze zostały sprawdzone przed nią), jesli znajdzie stację
% leżącą bliżej do S niż aktualnie najmniejsza odległość to zastępuje wcześniejszą
% parę S i tą stacją
closestTwo([S | T], {A, B, MinDist}) ->
  DistanceFromS = distanceFrom(S),
  [Closest | _] = lists:sort(fun(X, Y) -> DistanceFromS(X) > DistanceFromS(Y) end, T),
  Distance = DistanceFromS(Closest),
  if MinDist == 0; MinDist > Distance -> closestTwo(T, {S, Closest, Distance});
    true -> closestTwo(T, {A, B, MinDist})
  end;
%w przypadku błędnych argumentów
closestTwo(_, _) ->
  {error, wrong_arguments}.

%wrapper dla wcześniejszej funkcji
getTwoClosestStations(Monitor) ->
  {S1, S2, Dist} = closestTwo(Monitor, {empty, empty, 0}),
  {S1#station.name, S2#station.name, Dist}.
