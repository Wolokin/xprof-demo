-module(pollution_server).
-author("filip").

-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2,
  getDailyMean/2, getDailyOverLimit/3, getClosestStation/1, changeStationName/2]).

start() ->
  erlang:register(pollutionServer, spawn(fun init/0)).

stop() ->
  call(exit).

init() ->
  EmptyMonitor = pollution:createMonitor(),
  loop(EmptyMonitor).

call(Message) ->
  pollutionServer ! {request, erlang:self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

waitForCall(Monitor) ->
  receive
     {request, Pid, {addStation, {Name, Coords}}} ->
       {ok, Pid, pollution:addStation(Name, Coords, Monitor)};
     {request, Pid, {addValue, {Station, Datetime, MeasurementType, Value}}} ->
       {ok, Pid, pollution:addValue(Station, Datetime, MeasurementType, Value, Monitor)};
     {request, Pid, {removeValue, {Station, Datetime, MeasurementType}}} ->
       {ok, Pid, pollution:removeValue(Station, Datetime, MeasurementType, Monitor)};
     {request, Pid, {getOneValue, {Station, Datetime, MeasurementType}}} ->
       {pollution:getOneValue(Station, Datetime, MeasurementType, Monitor), Pid, Monitor};
     {request, Pid, {getStationMean, {Station, MeasurementType}}} ->
       {pollution:getStationMean(Station, MeasurementType, Monitor), Pid, Monitor};
     {request, Pid, {getDailyMean, {Date, MeasurementType}}} ->
       {pollution:getDailyMean(Date, MeasurementType, Monitor), Pid, Monitor};
     {request, Pid, {getDailyOverLimit, {Date, MeasurementType, Norm}}} ->
       {pollution:getDailyOverLimit(Date, MeasurementType, Norm, Monitor), Pid, Monitor};
     {request, Pid, {getClosestStation, {Coords}}} ->
       {pollution:getClosestStation(Coords, Monitor), Pid, Monitor};
     {request, Pid, {changeStationName, {OldName, NewName}}} ->
       {ok, Pid, pollution:changeStationName(OldName, NewName, Monitor)};
     {request, Pid, exit} ->
       {exit, Pid}
   end.

loop(Monitor) ->
  Result = waitForCall(Monitor),
  case Result of
    {exit, Pid} ->
      Pid ! {reply, ok};
    {Value, Pid, NewMonitor} ->
      Pid ! {reply, Value},
      loop(NewMonitor)
  end.

% Dodaje nowa stacje pomiarowa
addStation(Name, Coords) ->
  call({addStation, {Name, Coords}}).

% Dodaje pojedynczy pomiar o podanej wartosci do podanej stacji, daty i typu
addValue(Station, Datetime, MeasurementType, Value) ->
  call({addValue, {Station, Datetime, MeasurementType, Value}}).

% Usuwa pojedynczy pomiar z podanej stacji, daty i typu
removeValue(Station, Datetime, MeasurementType) ->
  call({removeValue, {Station, Datetime, MeasurementType}}).

% Zwraca wartosc pojedynczego pomiaru z podanej stacji, daty i typu
getOneValue(Station, Datetime, MeasurementType) ->
  call({getOneValue, {Station, Datetime, MeasurementType}}).

% Zwraca srednia wartosc pomiaru danego typu dla danej stacji
getStationMean(Station, MeasurementType) ->
  call({getStationMean, {Station, MeasurementType}}).

% Zwraca srednia wartosc danego parametru danego dnia na wszystkich stacjach
getDailyMean(Date, MeasurementType) ->
  call({getDailyMean, {Date, MeasurementType}}).

% Dodatkowa funkcjonalnosc
% Zwraca ilość stacji ktore danego dnia przekroczyly norme danego typu
getDailyOverLimit(Date, MeasurementType, Norm) ->
  call({getDailyOverLimit, {Date, MeasurementType, Norm}}).

% Dodatkowa funkcjonalnosc
% Zwraca nazwe najblizszej stacji w normie euklidesowej
getClosestStation(Coords) ->
  call({getClosestStation, {Coords}}).

% Dodatkowa funkcjonalnosc
% Zmienia nazwe stacji
changeStationName(OldName, NewName) ->
  call({changeStationName, {OldName, NewName}}).