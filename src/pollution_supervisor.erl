-module(pollution_supervisor).
-behaviour(supervisor).

-export([start/0, init/1]).

start() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, empty).
  %unlink(whereis(?MODULE)).

init(_) ->
  {ok, {
    #{strategy => one_for_one,
      intensity => 1,
      period => 1},
    [#{id => pollution_gen_server,
      start => {pollution_gen_server, start, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [pollution_gen_server]},
    #{id => pollution_value_collector_gen_statem,
      start => {pollution_value_collector_gen_statem, start, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [pollution_value_collector_gen_statem]}]}
  }.
