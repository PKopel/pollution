%%%-------------------------------------------------------------------
%%% @author pkopel
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2020 18:09
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("pkopel").

-include_lib("eunit/include/eunit.hrl").

% creating monitor structure:
create_test() ->
  ?assertEqual([], pollution:createMonitor()).

add_station_test() ->
  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, "Aleja Słowackiego", {50.2345, 18.3445}),
  ?assertEqual([
    {station, "Aleja Słowackiego", {50.2345, 18.3445}, []}
  ], M1),
  M2 = pollution:addStation(M1, "Nowa", {10, 10}),
  ?assertEqual([
    {station, "Nowa", {10, 10}, []},
    {station, "Aleja Słowackiego", {50.2345, 18.3445}, []}
  ], M2),
  % same station again
  Error = pollution:addStation(M2, "Aleja Słowackiego", {50.2345, 18.3445}),
  ?assertEqual({error, station_already_exists}, Error).

% recording measurements
add_value_test() ->
  M1 = [
    {station, "A", {1, 1}, []},
    {station, "B", {0, 0}, []}
  ],
  % in first station
  M2 = pollution:addValue(M1, {1, 1}, {{2020, 3, 30}, {24, 0, 0}}, "X", 61),
  ?assertEqual([
    {station, "A", {1, 1}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 61}
    ]},
    {station, "B", {0, 0}, []}
  ], M2),
  M3 = pollution:addValue(M2, "A", {{2020, 3, 30}, {24, 0, 0}}, "Y", 120),
  ?assertEqual([
    {station, "A", {1, 1}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 120},
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 61}
    ]},
    {station, "B", {0, 0}, []}
  ], M3),
  % in second station
  M4 = pollution:addValue(M3, "B", {{2020, 3, 30}, {24, 0, 0}}, "X", 59),
  ?assertEqual([
    {station, "A", {1, 1}, [

      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 120},
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 61}
    ]},
    {station, "B", {0, 0}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 59}
    ]}
  ], M4),
  M5 = pollution:addValue(M4, {0, 0}, {{2020, 3, 30}, {24, 0, 0}}, "Y", 121),
  ?assertEqual([
    {station, "A", {1, 1}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 120},
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 61}
    ]},
    {station, "B", {0, 0}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 121},
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 59}
    ]}
  ], M5),
  % same measurement again
  ?assertEqual({error, measurement_already_recorded}, pollution:addValue(M5, {1, 1}, {{2020, 3, 30}, {24, 0, 0}}, "X", 61)),
  % non-existing station
  ?assertEqual({error, no_such_station}, pollution:addValue(M5, "C", {{2020, 3, 30}, {24, 0, 0}}, "Z", 61)).

% reading measurements
get_one_test() ->
  M = [
    {station, "A", {0, 0}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 59},
      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 121}
    ]}
  ],
  ?assertEqual(59, pollution:getOneValue(M, {0, 0}, {{2020, 3, 30}, {24, 0, 0}}, "X")),
  ?assertEqual(121, pollution:getOneValue(M, "A", {{2020, 3, 30}, {24, 0, 0}}, "Y")),
  % non-existing measurement
  ?assertEqual({error, no_such_measurement}, pollution:getOneValue(M, "A", {{2020, 3, 10}, {24, 0, 0}}, "Y")),
  % non-existing station
  ?assertEqual({error, no_such_station}, pollution:getOneValue(M, "C", {{2020, 3, 10}, {24, 0, 0}}, "Y")).

% removing measurements
remove_test() ->
  M1 = [
    {station, "A", {0, 0}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 59}
    ]}
  ],
  M2 = pollution:removeValue(M1, "A", {{2020, 3, 30}, {24, 0, 0}}, "X"),
  ?assertEqual([{station, "A", {0, 0}, []}], M2),
  % non-existing measurement
  ?assertEqual({error, no_such_measurement}, pollution:removeValue(M2, "A", {{2020, 3, 30}, {24, 0, 0}}, "X")),
  % non-existing station
  ?assertEqual({error, no_such_station}, pollution:removeValue(M2, "C", {{2020, 3, 30}, {24, 0, 0}}, "X")).

% station means
station_mean_test() ->
  M = [
    {station, "A", {0, 0}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 59},
      {{{2020, 3, 30}, {12, 0, 0}}, "X", 121},
      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 121}
    ]}
  ],
  ?assertEqual(90.0, pollution:getStationMean(M, "A", "X")),
  ?assertEqual(121.0, pollution:getStationMean(M, {0, 0}, "Y")),
  % non-existing type
  ?assertEqual(0, pollution:getStationMean(M, {0, 0}, "Z")),
  % non-existing station
  ?assertEqual({error, no_such_station}, pollution:getStationMean(M, "B", "X")).

% daily means
daily_mean_test() ->
  M = [
    {station, "A", {1, 1}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 61},
      {{{2020, 3, 31}, {24, 0, 0}}, "X", 119},
      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 120}
    ]},
    {station, "B", {0, 0}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 59},
      {{{2020, 3, 31}, {24, 0, 0}}, "X", 121},
      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 120}
    ]}
  ],
  ?assertEqual(60.0, pollution:getDailyMean(M, "X", {2020, 3, 30})),
  ?assertEqual(120.0, pollution:getDailyMean(M, "X", {2020, 3, 31})),
  ?assertEqual(120.0, pollution:getDailyMean(M, "Y", {2020, 3, 30})),
  % non-existing type
  ?assertEqual(0, pollution:getDailyMean(M, "Z", {2020, 3, 30})),
  ?assertEqual(0, pollution:getDailyMean(M, "X", {2020, 3, 10})).


% two closest stations
closest_stations_test() ->
  M = [
    {station, "A", {4, 3}, []},
    {station, "B", {0, 0}, []},
    {station, "D", {-4, 3}, []},
    {station, "C", {10, 10}, []}
  ],
  ?assertEqual({"A", "B", 5.0}, pollution:getTwoClosestStations(M)).

% station with the lowest mean
min_type_mean_test() ->
  M = [
    {station, "A", {1, 1}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 61},
      {{{2020, 3, 31}, {24, 0, 0}}, "X", 119},
      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 110}
    ]},
    {station, "B", {0, 0}, [
      {{{2020, 3, 30}, {24, 0, 0}}, "X", 49},
      {{{2020, 3, 31}, {24, 0, 0}}, "X", 111},
      {{{2020, 3, 30}, {24, 0, 0}}, "Y", 120}
    ]}
  ],
  ?assertEqual({"B", 80.0}, pollution:getMinTypeMean(M, "X")),
  ?assertEqual({"A", 110.0}, pollution:getMinTypeMean(M, "Y")),
  % non-existing type
  ?assertEqual({error, no_such_measurement}, pollution:getMinTypeMean(M, "Z")).