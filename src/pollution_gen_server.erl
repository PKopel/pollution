%%%-------------------------------------------------------------------
%% @doc pollution public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0, stop/0, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2,
  getMinTypeMean/1, getTwoClosestStations/0, crash/0]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, pollution:createMonitor()}.

terminate(normal, _Monitor) ->
  io:format("Stopped monitor~n").
%% internal functions

%% interface

stop() ->
  gen_server:call(?MODULE, stop).

crash() ->
  gen_server:cast(?MODULE, crash).

addStation(Name, Coords) ->
  gen_server:call(?MODULE, {addStation, Name, Coords}).

addValue(Station, Date, Type, Value) ->
  gen_server:call(?MODULE, {addValue, Station, Date, Type, Value}).

removeValue(Station, Date, Type) ->
  gen_server:cast(?MODULE, {removeValue, Station, Date, Type}).

getOneValue(Station, Date, Type) ->
  gen_server:call(?MODULE, {getOneValue, Station, Date, Type}).

getDailyMean(Type, Date) ->
  gen_server:call(?MODULE, {getDailyMean, Date, Type}).

getStationMean(Station, Type) ->
  gen_server:call(?MODULE, {getStationMean, Station, Type}).

getMinTypeMean(Type) ->
  gen_server:call(?MODULE, {getMinTypeMean, Type}).

getTwoClosestStations() ->
  gen_server:call(?MODULE, getTwoClosestStations).

%% message handling

serve(Monitor, Function, Type) ->
  case Function(Monitor) of
    {error, Reason} -> {Type, {error, Reason}, Monitor};
    Result when is_list(Result) -> {Type, ok, Result};
    Result -> {Type, Result, Monitor}
  end.

handle_call({addStation, Name, Coords}, _From, Monitor) ->
  serve(Monitor, fun(M) -> pollution:addStation(M, Name, Coords) end, reply);
handle_call({addValue, Station, Date, Type, Value}, _From, Monitor) ->
  serve(Monitor, fun(M) -> pollution:addValue(M, Station, Date, Type, Value) end, reply);
handle_call({getOneValue, Station, Date, Type}, _From, Monitor) ->
  serve(Monitor, fun(M) -> pollution:getOneValue(M, Station, Date, Type) end, reply);
handle_call({getDailyMean, Date, Type}, _From, Monitor) ->
  serve(Monitor, fun(M) -> pollution:getDailyMean(M, Type, Date) end, reply);
handle_call({getStationMean, Station, Type}, _From, Monitor) ->
  serve(Monitor, fun(M) -> pollution:getStationMean(M, Station, Type) end, reply);
handle_call({getMinTypeMean, Type}, _From, Monitor) ->
  serve(Monitor, fun(M) -> pollution:getMinTypeMean(M, Type) end, reply);
handle_call(getTwoClosestStations, _From, Monitor) ->
  serve(Monitor, fun(M) -> pollution:getTwoClosestStations(M) end, reply);
handle_call(stop, _From, Monitor) ->
  {stop, normal, ok, Monitor}.

handle_cast({removeValue, Station, Date, Type}, Monitor) ->
  serve(Monitor, fun(M) -> pollution:removeValue(M, Station, Date, Type) end, noreply);
handle_cast(crash, Monitor) ->
  pollution:crash(),
  {noreply, Monitor}.

handle_info(_Info, Monitor) ->
  {noreply, Monitor}.