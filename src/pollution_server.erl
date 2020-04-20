%%%-------------------------------------------------------------------
%%% @author pkopel
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2020 16:55
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("pkopel").

%% API
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2,
  getMinTypeMean/1, getTwoClosestStations/0]).
-export([init/0]).

start() ->
  register(pollutionServer, spawn(?MODULE, init, [])).

init() ->
  loop(pollution:createMonitor()).

serve(Monitor, Function, Sender) ->
  case Function(Monitor) of
    {error, Reason} -> Sender ! {reply, {error, Reason}}, loop(Monitor);
    Result when is_list(Result) -> Sender ! {reply, ok}, loop(Result);
    Result -> Sender ! {reply, {ok, Result}}, loop(Monitor)
  end.


loop(Monitor) ->
  receive
    {request, Sender, {addStation, Name, Coords}} ->
      serve(Monitor, fun(M) -> pollution:addStation(M, Name, Coords) end, Sender);
    {request, Sender, {addValue, Station, Date, Type, Value}} ->
      serve(Monitor, fun(M) -> pollution:addValue(M, Station, Date, Type, Value) end, Sender);
    {request, Sender, {removeValue, Station, Date, Type}} ->
      serve(Monitor, fun(M) -> pollution:removeValue(M, Station, Date, Type) end, Sender);
    {request, Sender, {getOneValue, Station, Date, Type}} ->
      serve(Monitor, fun(M) -> pollution:getOneValue(M, Station, Date, Type) end, Sender);
    {request, Sender, {getDailyMean, Date, Type}} ->
      serve(Monitor, fun(M) -> pollution:getDailyMean(M, Type, Date) end, Sender);
    {request, Sender, {getStationMean, Station, Type}} ->
      serve(Monitor, fun(M) -> pollution:getStationMean(M, Station, Type) end, Sender);
    {request, Sender, {getMinTypeMean, Type}} ->
      serve(Monitor, fun(M) -> pollution:getMinTypeMean(M, Type) end, Sender);
    {request, Sender, getTwoClosestStations} ->
      serve(Monitor, fun(M) -> pollution:getTwoClosestStations(M) end, Sender);
    {request, Sender, stop} -> Sender ! {reply, ok}
  end.

call(Message) ->
  pollutionServer ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  after 20000 -> timeout
  end.

stop() ->
  call(stop).

addStation(Name, Coords) ->
  call({addStation, Name, Coords}).

addValue(Station, Date, Type, Value) ->
  call({addValue, Station, Date, Type, Value}).

removeValue(Station, Date, Type) ->
  call({removeValue, Station, Date, Type}).

getOneValue(Station, Date, Type) ->
  call({getOneValue, Station, Date, Type}).

getDailyMean(Date, Type) ->
  call({getDailyMean, Date, Type}).

getStationMean(Station, Type) ->
  call({getStationMean, Station, Type}).

getMinTypeMean(Type) ->
  call({getMinTypeMean, Type}).

getTwoClosestStations() ->
  call(getTwoClosestStations).