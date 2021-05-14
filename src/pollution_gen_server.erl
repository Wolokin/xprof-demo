-module(pollution_gen_server).
-behaviour(gen_server).
-author("filip").

-export([start/0, stop/0, crash/0, terminate/2]).
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDailyOverLimit/3, getClosestStation/1, changeStationName/2, getSortedStations1/0, getSortedStations2/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start() ->
  gen_server:start_link(
    {local, pollution_gen_server},
    pollution_gen_server,
    empty,
    []).

stop() ->
  gen_server:cast(pollution_gen_server, {stop}).

crash() ->
  gen_server:call(pollution_gen_server, {crash}).

terminate(Reason, Monitor) ->
  io:format("Server exit. Monitor: ~w~n", [Monitor]),
  Reason.

init(_) ->
  {ok, pollution:createMonitor()}.

addStation(Name, Coords) ->
  gen_server:call(pollution_gen_server, {addStation, {Name, Coords}}).
addValue(Station, Datetime, MeasurementType, Value) ->
  gen_server:call(pollution_gen_server, {addValue, {Station, Datetime, MeasurementType, Value}}).
removeValue(Station, Datetime, MeasurementType) ->
  gen_server:call(pollution_gen_server, {removeValue, {Station, Datetime, MeasurementType}}).
getOneValue(Station, Datetime, MeasurementType) ->
  gen_server:call(pollution_gen_server, {getOneValue, {Station, Datetime, MeasurementType}}).
getStationMean(Station, MeasurementType) ->
  gen_server:call(pollution_gen_server, {getStationMean, {Station, MeasurementType}}).
getDailyMean(Date, MeasurementType) ->
  gen_server:call(pollution_gen_server, {getDailyMean, {Date, MeasurementType}}).
getDailyOverLimit(Date, MeasurementType, Norm) ->
  gen_server:call(pollution_gen_server, {getDailyOverLimit, {Date, MeasurementType, Norm}}).
getClosestStation(Coords) ->
  gen_server:call(pollution_gen_server, {getClosestStation, {Coords}}).
changeStationName(OldName, NewName) ->
  gen_server:call(pollution_gen_server, {changeStationName, {OldName, NewName}}).
getSortedStations1() ->
  gen_server:call(pollution_gen_server, {getSortedStations1}).
getSortedStations2() ->
  gen_server:call(pollution_gen_server, {getSortedStations2}).

check_for_bad_modification(NewMonitor, Monitor) ->
  case NewMonitor of
    {error, _} = Error -> {reply, Error, Monitor};
    _ -> {reply, ok, NewMonitor}
  end.

handle_call({addStation, {Name, Coords}}, _, Monitor) ->
  check_for_bad_modification(pollution:addStation(Name, Coords, Monitor), Monitor);
handle_call({addValue, {Station, Datetime, MeasurementType, Value}}, _, Monitor) ->
  check_for_bad_modification(pollution:addValue(Station, Datetime, MeasurementType, Value, Monitor), Monitor);
handle_call({removeValue, {Station, Datetime, MeasurementType}}, _, Monitor) ->
  check_for_bad_modification(pollution:removeValue(Station, Datetime, MeasurementType, Monitor), Monitor);
handle_call({getOneValue, {Station, Datetime, MeasurementType}}, _, Monitor) ->
  {reply, pollution:getOneValue(Station, Datetime, MeasurementType, Monitor), Monitor};
handle_call({getStationMean, {Station, MeasurementType}}, _, Monitor) ->
  {reply, pollution:getStationMean(Station, MeasurementType, Monitor), Monitor};
handle_call({getDailyMean, {Date, MeasurementType}}, _, Monitor) ->
  {reply, pollution:getDailyMean(Date, MeasurementType, Monitor), Monitor};
handle_call({getDailyOverLimit, {Date, MeasurementType, Norm}}, _, Monitor) ->
  {reply, pollution:getDailyOverLimit(Date, MeasurementType, Norm, Monitor), Monitor};
handle_call({getClosestStation, {Coords}}, _, Monitor) ->
  {reply, pollution:getClosestStation(Coords,Monitor), Monitor};
handle_call({changeStationName, {OldName, NewName}}, _, Monitor) ->
  check_for_bad_modification(pollution:changeStationName(OldName,NewName, Monitor), Monitor);
handle_call({getSortedStations1}, _, Monitor) ->
  {reply, pollution:getSortedStations1(Monitor), Monitor};
handle_call({getSortedStations2}, _, Monitor) ->
  {reply, pollution:getSortedStations2(Monitor), Monitor}.

handle_cast({stop}, Monitor) ->
  {stop, normal, Monitor};
handle_cast({crash}, Monitor) ->
  {noreply, non:existent(), Monitor}.
