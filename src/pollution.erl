-module(pollution).
-author("Filip").

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getClosestStation/2, getDailyOverLimit/4, changeStationName/3]).
-export([getSortedStations1/1, getSortedStations2/1]).
-record(monitor, {coordsToName, nameToData}).

% Funkcja pomocnicza, zwraca dane stacji jako klucz przyjmujac nazwe badz wspolrzedne stacji
getStationData(Coords, Monitor) when erlang:is_tuple(Coords) ->
  #monitor{coordsToName = CoordsToName} = Monitor,
  case maps:is_key(Coords, CoordsToName) of
    true -> #{Coords := Name} = CoordsToName,
      getStationData(Name, Monitor);
    false -> {error, no_station_found}
  end;
getStationData(Name, Monitor) ->
  #monitor{nameToData = NameToData} = Monitor,
  case maps:is_key(Name, NameToData) of
    true -> #{Name := Data} = NameToData,
      {Name, Data};
    false -> {error, no_station_found}
  end.
checkForErrors(StationData, Action) ->
  case StationData of
    {error, _} -> StationData;
    _ -> Action()
  end.

% Funkcja pomocnicza, wstawia zauktualizowane dane pomiarowe pojedynczej stacji do monitora
putStationData(Name, Data, #monitor{nameToData = OldData} = Monitor) ->
  Monitor#monitor{nameToData = OldData#{Name => Data}}.

% Funkcja pomocnicza, zwraca srednia wartosc w liscie
listAvg([]) -> 0;
listAvg(List) -> lists:sum(List)/erlang:length(List).

% Tworzy nowy monitor zanieczyszczen
% Zwraca rekord #monitor, ktory posiada dwa pola:
%   -coordsToName: mapa koordynatow na nazwe stacji
%   -nameToData: mapa nazw stacji na dane pomiarowe
% Dane pomiarowe sa przechowywane jako mapa {Data, TypPomiaru} => WartoscPomiaru
% Schemat struktury monitora:
% #monitor{coordsToName = #{Wspolrzedne => Nazwa},
%          nameToData : #{Nazwa => #{{Data, TypPomiaru} => WartoscPomiaru}}}
createMonitor() -> #monitor{coordsToName = #{}, nameToData = #{}}.

% Dodaje nowa stacje pomiarowa
addStation(Name, Coords, #monitor{coordsToName = C, nameToData = N}) ->
  case {maps:is_key(Coords, C), maps:is_key(Name, N)} of
    {true, _} -> {error, duplicate_coords};
    {_, true} -> {error, duplicate_name};
    _ -> #monitor{coordsToName = C#{Coords => Name}, nameToData = N#{Name => #{}}}
  end.

% Dodaje pojedynczy pomiar o podanej wartosci do podanej stacji, daty i typu
addValue(Station, Datetime, MeasurementType, Value, Monitor) ->
  {Name, Data} = getStationData(Station, Monitor),
  Key = {Datetime, MeasurementType},
  checkForErrors({Name, Data},
    fun() -> case maps:is_key(Key, Data) of
                false -> putStationData(Name, Data#{Key => Value}, Monitor);
                _ -> {error, duplicate_measurement}
             end
    end).


% Usuwa pojedynczy pomiar z podanej stacji, daty i typu
removeValue(Station, Datetime, MeasurementType, Monitor) ->
  {Name, Data} = getStationData(Station, Monitor),
  Key = {Datetime, MeasurementType},
  case {Name, maps:is_key(Key, Data)} of
    {error, _} -> {Name, Data};
    {_, false} -> {error, measurement_not_found};
    _ -> putStationData(Name, maps:remove(Key, Data), Monitor)
  end.

% Zwraca wartosc pojedynczego pomiaru z podanej stacji, daty i typu
getOneValue(Station, Datetime, MeasurementType, Monitor) ->
  {Name, Data} = getStationData(Station, Monitor),
  Key = {Datetime, MeasurementType},
  checkForErrors({Name, Data},
    fun() ->
      maps:get(Key, Data, {error, measurement_not_found})
    end).

% Zwraca srednia wartosc pomiaru danego typu dla danej stacji
getStationMean(Station, MeasurementType, Monitor) ->
  {Name, Data} = getStationData(Station, Monitor),
  checkForErrors({Name, Data},
    fun() ->
    listAvg(
      maps:values(
        maps:filter(fun({_, M},_) -> M == MeasurementType end, Data)))
    end).

% Funkcja pomocnicza - zwraca nameToData w ktorym wystepuja tylko dane spelniajace Func
% Func musi dzialac na mapie {Date, MeasurementType} -> Value
filterEntriesInAllStations(Func, NameToData) ->
  maps:map(fun(_, Data) -> maps:filter(Func, Data) end, NameToData).

% Funkcja pomocnicza - majac pojedynczy pomiar stwierdza czy jego data i typ sa zgodne z podanymi
% Przydatne w polaczeniu z filterEntriesInAllStations
equalDateAndType(Date, MeasurementType) ->
  fun ({{D, _}, M}, _) -> Date == D andalso M == MeasurementType end.

% Zwraca srednia wartosc danego parametru danego dnia na wszystkich stacjach
getDailyMean(Date, MeasurementType, #monitor{nameToData = N}) ->
  F = filterEntriesInAllStations(equalDateAndType(Date, MeasurementType), N),
  Res = lists:map(fun maps:values/1, maps:values(F)),
  listAvg(lists:flatten(Res)).

distance2({A1, B1}, {A2, B2}) ->
  (A1-A2)*(A1-A2) + (B1-B2)*(B1-B2).

% Dodatkowa funkcjonalnosc
% Zwraca ilość stacji ktore danego dnia przekroczyly norme danego typu
getDailyOverLimit(Date, MeasurementType, Norm, #monitor{nameToData = N}) ->
  F = filterEntriesInAllStations(equalDateAndType(Date, MeasurementType), N),
  F2 = filterEntriesInAllStations(fun (_, V) -> V > Norm end, F),
  erlang:length(lists:filter(fun (E) -> maps:size(E) > 0 end, maps:values(F2))).

% Dodatkowa funkcjonalnosc
% Zwraca nazwe najblizszej stacji w normie euklidesowej
getClosestStation(Coords, #monitor{coordsToName = C}) ->
  Dist = maps:map(fun (K,_) -> distance2(Coords, K) end, C),
  {_, MinKey} = maps:fold(fun (K, V, {Acc, Kcc}) ->
    case V < Acc of
      true -> {V, K};
      _ -> {Acc, Kcc}
    end end, {1000000, 0}, Dist),
  maps:get(MinKey, C).

% Dodatkowa funkcjonalnosc
% Zmienia nazwe stacji
changeStationName(OldName, NewName, #monitor{coordsToName = C, nameToData = N} = Monitor) ->
  List = maps:to_list(C),
  Found = lists:keyfind(OldName, 2, List),
  case Found of
    false -> {error, station_not_found};
    _ -> {Coords, _} = Found,
      C2 = C#{Coords := NewName},
      {_, Data} = getStationData(OldName, Monitor),
      N2 = maps:remove(OldName, N),
      M2 = #monitor{coordsToName = C2, nameToData = N2},
      putStationData(NewName, Data, M2)
  end.






















% Demo
lessThan(List, Arg) -> [X || X <- List, X < Arg].
grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].
qs([]) -> [];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).
getSortedStations1(#monitor{coordsToName = C}) ->
  qs(maps:values(C)).
getSortedStations2(#monitor{coordsToName = C}) ->
  lists:sort(maps:values(C)).
