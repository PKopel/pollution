%%%-------------------------------------------------------------------
%% @doc pollution_r top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link( ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_all,
    intensity => 1,
    period => 1},
  ChildSpecs = [#{
    id => pollution_gen_server,
    start => {pollution_gen_server, start_link, []},
    restart => transient,
    shutdown => brutal_kill,
    type => worker,
    modules => [pollution_gen_server, pollution]
  }],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
