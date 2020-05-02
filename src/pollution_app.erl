%%%-------------------------------------------------------------------
%% @doc pollution public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2,
  getMinTypeMean/1, getTwoClosestStations/0, crash/0]).

start(_StartType, _StartArgs) ->
    pollution_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

stop() ->
  pollution_gen_server:stop().

crash() ->
  pollution_gen_server:crash().

addStation(Name, Coords) ->
  pollution_gen_server:addStation( Name, Coords).

addValue(Station, Date, Type, Value) ->
  pollution_gen_server:addValue( Station, Date, Type, Value).

removeValue(Station, Date, Type) ->
  pollution_gen_server:removeValue( Station, Date, Type).

getOneValue(Station, Date, Type) ->
  pollution_gen_server:getOneValue( Station, Date, Type).

getDailyMean(Type, Date) ->
  pollution_gen_server:getDailyMean( Date, Type).

getStationMean(Station, Type) ->
  pollution_gen_server:getStationMean( Station, Type).

getMinTypeMean(Type) ->
  pollution_gen_server:getMinTypeMean( Type).

getTwoClosestStations() ->
  pollution_gen_server:getTwoClosestStations().