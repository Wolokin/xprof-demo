-module(clients).

-export([start/1, client/1]).

start(mean) ->
    spawn(?MODULE, client, [mean]);
start(name) ->
    spawn(?MODULE, client, [name]);
start(sort) ->
    spawn(?MODULE, client, [sort]).

client(mean) ->
    case rand:uniform() < 0.1 of
        true -> pollution_gen_server:getStationMean({20.0,50.0}, list_to_binary("PM10"));
        false -> pollution_gen_server:getStationMean({19.992,50.081}, list_to_binary("PM10"))
    end,
    timer:sleep(100),
    client(mean);
client(name) ->
%    Reply = pollution_gen_server:changeStationName(station_20_50, big_station),
    Reply = pollution_gen_server:changeStationName(list_to_binary("station_20.0_50.0"), list_to_binary("big_station")),
    case Reply of
        {error, _} -> timer:sleep(10),
                      client(name);
        _ -> ok
    end;
client(sort) ->
    pollution_gen_server:getSortedStations1(),
    timer:sleep(100),
    client(sort2);
client(sort2) ->
    pollution_gen_server:getSortedStations2(),
    timer:sleep(100),
    client(sort).
