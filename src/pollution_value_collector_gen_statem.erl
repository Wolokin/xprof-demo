-module(pollution_value_collector_gen_statem).
-behavior(gen_statem).
-author("filip").

%% A slightly more advanced gen_statem than required -
%% can accumulate data for multiple stations and flush them all at once

-export([init/1, callback_mode/0, start/0, stop/0, terminate/3]).
-export([initial_state/3, add_values/3]).
-export([setStation/1, addValue/3, storeData/0]).

server_name() -> ?MODULE.

start() ->
  gen_statem:start_link({local, server_name()}, ?MODULE, [], []).
stop() ->
  gen_statem:stop(server_name()).

init([]) ->
  {ok, initial_state, #{}}.

callback_mode() ->
  state_functions.

terminate(Reason, StateName, State) ->
  io:format("Statem terminated ~w~n", [[StateName, State]]),
  Reason.

setStation(StationNameOrCoords) ->
  gen_statem:cast(server_name(), {set_station, StationNameOrCoords}).
addValue(Datetime, MeasurementType, Value) ->
  gen_statem:cast(server_name(), {add_values, {Datetime, MeasurementType, Value}}).
storeData() ->
  gen_statem:cast(server_name(), store_data).

initial_state(_Event, {set_station, StationNameOrCoords}, #{}) ->
  {next_state, add_values, #{current_station => StationNameOrCoords,
                                data => #{StationNameOrCoords => []}}}.

add_values(_Event, {add_values, Value}, #{current_station := Station} = State) ->
  #{data := #{Station := Values} = Data} = State,
  {next_state, add_values, State#{data => Data#{Station => [Value | Values]}}};
add_values(_Event, {set_station, StationNameOrCoords}, #{data := Data}) ->
  StationValues = case maps:is_key(StationNameOrCoords, Data) of
    true -> maps:get(StationNameOrCoords, Data);
    false -> []
  end,
  {next_state, add_values, #{current_station => StationNameOrCoords, data => Data#{StationNameOrCoords => StationValues}}};
add_values(_Event, store_data, #{data := Data}) ->
  io:format("Storing ~p~n", [[{Station, add_batch(Station, Values)} || {Station, Values} <- maps:to_list(Data)]]),
  {next_state, initial_state, #{}}.

add_batch(_, []) ->
  ok;
add_batch(StationNameOrCoords, [{Datetime, MeasurementType, Value} | Values]) ->
  pollution_gen_server:addValue(StationNameOrCoords, Datetime, MeasurementType, Value),
  add_batch(StationNameOrCoords, Values).
