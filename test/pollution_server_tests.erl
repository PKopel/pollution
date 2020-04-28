%%%-------------------------------------------------------------------
%%% @author pkopel
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2020 18:10
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("pkopel").

-include_lib("eunit/include/eunit.hrl").

% dodawanie pomiarów
pollution_server_test_() ->
  {setup,
    fun pollution_server:start/0,
    fun(_) -> pollution_server:stop() end,
    fun(Data) -> {inorder, [
      add_station_instantiator(Data),
      add_value_instantiator(Data),
      remove_instantiator(Data),
      get_one_instantiator(Data),
      station_mean_instantiator(Data),
      daily_mean_instantiator(Data),
      min_type_mean_instantiator(Data),
      closest_stations_instantiator(Data)
    ]} end
  }.

add_station_instantiator(_) ->
  [
    ?_assertEqual(ok, pollution_server:addStation("A", {1, 1})),
    ?_assertEqual(ok, pollution_server:addStation("B", {5, 4})),
    ?_assertEqual(ok, pollution_server:addStation("C", {10, 10})),
    % kolejny raz ta sama
    ?_assertEqual({error, station_already_exists}, pollution_server:addStation("A", {1, 1}))
  ].

add_value_instantiator(_) ->
  [% dodanie pomiarów do pierwszej stacji
    ?_assertEqual(ok, pollution_server:addValue({1, 1}, {{2020, 3, 30}, {24, 0, 0}}, "X", 61)),
    ?_assertEqual(ok, pollution_server:addValue({1, 1}, {{2020, 3, 31}, {24, 0, 0}}, "X", 39)),
    ?_assertEqual(ok, pollution_server:addValue("A", {{2020, 3, 30}, {24, 0, 0}}, "Y", 119)),
    ?_assertEqual(ok, pollution_server:addValue("A", {{2020, 3, 31}, {24, 0, 0}}, "Y", 101)),
    ?_assertEqual(ok, pollution_server:addValue("A", {{2020, 4, 1}, {24, 0, 0}}, "X", 49)),
    % dodanie pomiarów do drugiej stacji
    ?_assertEqual(ok, pollution_server:addValue("B", {{2020, 3, 30}, {24, 0, 0}}, "X", 59)),
    ?_assertEqual(ok, pollution_server:addValue("B", {{2020, 3, 31}, {24, 0, 0}}, "X", 41)),
    ?_assertEqual(ok, pollution_server:addValue({5, 4}, {{2020, 3, 30}, {24, 0, 0}}, "Y", 121)),
    ?_assertEqual(ok, pollution_server:addValue({5, 4}, {{2020, 3, 31}, {24, 0, 0}}, "Y", 99)),
    % kolejny raz to samo
    ?_assertEqual({error, measurement_already_recorded}, pollution_server:addValue({1, 1}, {{2020, 3, 30}, {24, 0, 0}}, "X", 61)),
    % nieistniejąca stacja
    ?_assertEqual({error, no_such_station}, pollution_server:addValue("D", {{2020, 3, 30}, {24, 0, 0}}, "Z", 61))
  ].

% sprawdzenie zapisów
get_one_instantiator(_) ->
  [
    ?_assertEqual({ok, 59}, pollution_server:getOneValue({5, 4}, {{2020, 3, 30}, {24, 0, 0}}, "X")),
    ?_assertEqual({ok, 119}, pollution_server:getOneValue("A", {{2020, 3, 30}, {24, 0, 0}}, "Y")),
    % nieistniejący pomiar
    ?_assertEqual({error, no_such_measurement}, pollution_server:getOneValue("A", {{2020, 3, 10}, {24, 0, 0}}, "Y")),
    % nieistniejaca stacja
    ?_assertEqual({error, no_such_station}, pollution_server:getOneValue("D", {{2020, 3, 10}, {24, 0, 0}}, "Y"))
  ].

% usunięcie pomiaru
remove_instantiator(_) ->
  [
    ?_assertEqual(ok, pollution_server:removeValue("A", {{2020, 4, 1}, {24, 0, 0}}, "X")),
    % nieistniejący pomiar
    ?_assertEqual({error, no_such_measurement}, pollution_server:removeValue("A", {{2020, 3, 30}, {12, 0, 0}}, "X")),
    % nieistniejąca stacja
    ?_assertEqual({error, no_such_station}, pollution_server:removeValue("D", {{2020, 3, 30}, {24, 0, 0}}, "X"))
  ].

% sprawdzenie średnich dla stacji
station_mean_instantiator(_) ->
  [
    ?_assertEqual({ok, 50.0}, pollution_server:getStationMean("A", "X")),
    ?_assertEqual({ok, 110.0}, pollution_server:getStationMean({5, 4}, "Y")),
    % nieistniejący pomiar
    ?_assertEqual({ok, 0}, pollution_server:getStationMean({5, 4}, "Z")),
    % nieistniejąca stacja
    ?_assertEqual({error, no_such_station}, pollution_server:getStationMean("D", "X"))
  ].

% sprawdzenie średnich dla dni
daily_mean_instantiator(_) ->
  [
    ?_assertEqual({ok, 60.0}, pollution_server:getDailyMean("X", {2020, 3, 30})),
    ?_assertEqual({ok, 40.0}, pollution_server:getDailyMean("X", {2020, 3, 31})),
    ?_assertEqual({ok, 120.0}, pollution_server:getDailyMean("Y", {2020, 3, 30})),
    % nieistniejący pomiar
    ?_assertEqual({ok, 0}, pollution_server:getDailyMean("Z", {2020, 3, 30})),
    ?_assertEqual({ok, 0}, pollution_server:getDailyMean("X", {2020, 3, 10}))
  ].


% stacje najbliżej siebie
closest_stations_instantiator(_) ->
  ?_assertEqual({ok, {"B", "A", 5.0}}, pollution_server:getTwoClosestStations()).

% stacja o najniższym średnim odczycie danego typu
min_type_mean_instantiator(_) ->
  [
    ?_assertEqual({ok, {"B", 50.0}}, pollution_server:getMinTypeMean("X")),
    ?_assertEqual({ok, {"B", 110.0}}, pollution_server:getMinTypeMean("Y")),
    % nieistniejący typ
    ?_assertEqual({error, no_such_measurement}, pollution_server:getMinTypeMean("Z"))
  ].