%%%-------------------------------------------------------------------
%%% @author pkopel
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2020 18:10
%%%-------------------------------------------------------------------
-module(pollution_gen_server_tests).
-author("pkopel").

-include_lib("eunit/include/eunit.hrl").

pollution_gen_server_test_() ->
  {setup,
    fun pollution_gen_server:start_link/0,
    fun(_) -> pollution_gen_server:stop() end,
    fun(Data) -> {inorder, [
      add_station_instantiator(Data),
      add_value_instantiator(Data),
      remove_instantiator(Data),
      get_one_instantiator(Data),
      station_mean_instantiator(Data),
      daily_mean_instantiator(Data),
      min_type_mean_instantiator(Data),
      closest_stations_instantiator(Data)%%,
      %%crash_instantiator(Data)
    ]} end
  }.

add_station_instantiator(_) ->
  [
    ?_assertEqual(ok, pollution_gen_server:addStation("A", {1, 1})),
    ?_assertEqual(ok, pollution_gen_server:addStation("B", {5, 4})),
    ?_assertEqual(ok, pollution_gen_server:addStation("C", {10, 10})),
    % add the same station again
    ?_assertEqual({error, station_already_exists}, pollution_gen_server:addStation("A", {1, 1}))
  ].

add_value_instantiator(_) ->
  [% add measurements to the first station
    ?_assertEqual(ok, pollution_gen_server:addValue({1, 1}, {{2020, 3, 30}, {24, 0, 0}}, "X", 61)),
    ?_assertEqual(ok, pollution_gen_server:addValue({1, 1}, {{2020, 3, 31}, {24, 0, 0}}, "X", 39)),
    ?_assertEqual(ok, pollution_gen_server:addValue("A", {{2020, 3, 30}, {24, 0, 0}}, "Y", 119)),
    ?_assertEqual(ok, pollution_gen_server:addValue("A", {{2020, 3, 31}, {24, 0, 0}}, "Y", 101)),
    ?_assertEqual(ok, pollution_gen_server:addValue("A", {{2020, 4, 1}, {24, 0, 0}}, "X", 49)),
    % add measurements to the second station
    ?_assertEqual(ok, pollution_gen_server:addValue("B", {{2020, 3, 30}, {24, 0, 0}}, "X", 59)),
    ?_assertEqual(ok, pollution_gen_server:addValue("B", {{2020, 3, 31}, {24, 0, 0}}, "X", 41)),
    ?_assertEqual(ok, pollution_gen_server:addValue({5, 4}, {{2020, 3, 30}, {24, 0, 0}}, "Y", 121)),
    ?_assertEqual(ok, pollution_gen_server:addValue({5, 4}, {{2020, 3, 31}, {24, 0, 0}}, "Y", 99)),
    % add measurement again
    ?_assertEqual({error, measurement_already_recorded}, pollution_gen_server:addValue({1, 1}, {{2020, 3, 30}, {24, 0, 0}}, "X", 61)),
    % non-existing station
    ?_assertEqual({error, no_such_station}, pollution_gen_server:addValue("D", {{2020, 3, 30}, {24, 0, 0}}, "Z", 61))
  ].

% recording measurement
get_one_instantiator(_) ->
  [
    ?_assertEqual( 59, pollution_gen_server:getOneValue({5, 4}, {{2020, 3, 30}, {24, 0, 0}}, "X")),
    ?_assertEqual( 119, pollution_gen_server:getOneValue("A", {{2020, 3, 30}, {24, 0, 0}}, "Y")),
    % non-existing measurement
    ?_assertEqual({error, no_such_measurement}, pollution_gen_server:getOneValue("A", {{2020, 3, 10}, {24, 0, 0}}, "Y")),
    % non-existing station
    ?_assertEqual({error, no_such_station}, pollution_gen_server:getOneValue("D", {{2020, 3, 10}, {24, 0, 0}}, "Y"))
  ].

% removing measurement
remove_instantiator(_) ->
  [
    ?_assertEqual(ok, pollution_gen_server:removeValue("A", {{2020, 4, 1}, {24, 0, 0}}, "X")),
    % non-existing measurement
    ?_assertEqual({error, no_such_measurement}, pollution_gen_server:removeValue("A", {{2020, 3, 30}, {12, 0, 0}}, "X")),
    % non-existing station
    ?_assertEqual({error, no_such_station}, pollution_gen_server:removeValue("D", {{2020, 3, 30}, {24, 0, 0}}, "X"))
  ].

% station means
station_mean_instantiator(_) ->
  [
    ?_assertEqual( 50.0, pollution_gen_server:getStationMean("A", "X")),
    ?_assertEqual( 110.0, pollution_gen_server:getStationMean({5, 4}, "Y")),
    % non-existing measurement
    ?_assertEqual(0, pollution_gen_server:getStationMean({5, 4}, "Z")),
    % non-existing station
    ?_assertEqual({error, no_such_station}, pollution_gen_server:getStationMean("D", "X"))
  ].

% daily means
daily_mean_instantiator(_) ->
  [
    ?_assertEqual(60.0, pollution_gen_server:getDailyMean("X", {2020, 3, 30})),
    ?_assertEqual(40.0, pollution_gen_server:getDailyMean("X", {2020, 3, 31})),
    ?_assertEqual(120.0, pollution_gen_server:getDailyMean("Y", {2020, 3, 30})),
    % non-existing measurement
    ?_assertEqual( 0, pollution_gen_server:getDailyMean("Z", {2020, 3, 30})),
    ?_assertEqual( 0, pollution_gen_server:getDailyMean("X", {2020, 3, 10}))
  ].


% stations closest to each other
closest_stations_instantiator(_) ->
  ?_assertEqual({"B", "A", 5.0}, pollution_gen_server:getTwoClosestStations()).

% station with lowest type mean
min_type_mean_instantiator(_) ->
  [
    ?_assertEqual({"B", 50.0}, pollution_gen_server:getMinTypeMean("X")),
    ?_assertEqual({"B", 110.0}, pollution_gen_server:getMinTypeMean("Y")),
    % non-existing type
    ?_assertEqual({error, no_such_measurement}, pollution_gen_server:getMinTypeMean("Z"))
  ].

