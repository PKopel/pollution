%%%-------------------------------------------------------------------
%% @doc pollution_r public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_r_gen_server).

-behaviour(gen_serverg).

-export([start/2, stop/1, init/1, handle_call/3, handle_cast/2]).

start(_StartType, _StartArgs) ->
    pollution_r_sup:start_link().

stop(_State) ->
    ok.

%% internal functions


init(Args) ->
  erlang:error(not_implemented).

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).