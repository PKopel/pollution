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

start() ->
  register(pollutionServer, spawn(pollution_server, init, [])).

init() ->
  loop(pollution:createMonitor()).

loop(Monitor) ->
  receive
    {request, Sender, {addStation, Name, Coords}} ->
      Result = pollution:addStation(Monitor, Name, Coords),

  after 200000 -> timeout
  end.

call(Message) ->
  pollutionServer ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  after 20000 -> timeout
  end.