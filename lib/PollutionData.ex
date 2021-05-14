defmodule PollutionData do
  def timeAll() do
    us = 1_000_000
    {readings, coordsToName} = loadFile("pollution.csv")

    t_loadStations =
      :timer.tc(&flushStations/1, [coordsToName])
      |> elem(0)

    t_loadMeasurements =
      :timer.tc(&flushMeasurements/1, [readings])
      |> elem(0)

    {t_getStationMean, v1} =
      :timer.tc(&:pollution_gen_server.getStationMean/2, [{20.06, 49.986}, "PM10"])

    {t_getDailyMean, v2} =
      :timer.tc(
        &:pollution_gen_server.getDailyMean/2,
        [{2017, 5, 3}, "PM10"]
      )

    {t_loadStations / us, t_loadMeasurements / us, {t_getStationMean / us, v1},
     {t_getDailyMean / us, v2}}
  end
  
  def loadData() do
    _ = timeAll()
    us = 1_000_000
    {readings, coordsToName} = loadFile("bigdata.csv")

    t_loadStations =
      :timer.tc(&flushStations/1, [coordsToName])
      |> elem(0)

    t_loadMeasurements =
      :timer.tc(&flushMeasurements/1, [readings])
      |> elem(0)

    {t_getStationMean, v1} =
      :timer.tc(&:pollution_gen_server.getStationMean/2, [{20.06, 49.986}, "PM10"])

    {t_getDailyMean, v2} =
      :timer.tc(
        &:pollution_gen_server.getDailyMean/2,
        [{2017, 5, 3}, "PM10"]
      )

    {t_loadStations / us, t_loadMeasurements / us, {t_getStationMean / us, v1},
     {t_getDailyMean / us, v2}}
     :ok
  end

  def loadFile(name) do
    readings =
      importLinesFromCSV(name)
      |> Enum.map(&convertLine/1)

    coordsToName =
      findStations(readings)
      |> Enum.map(fn x -> {x, nameStation(x)} end)
      |> Enum.into(%{})

    {readings, coordsToName}
  end

  def importLinesFromCSV(name) do
    File.read!(name)
    |> String.split("\n")
  end

  def int_parse(str) do
    Integer.parse(str)
    |> elem(0)
  end

  def convertLine(line) do
    [date, time, loc1, loc2, val] = String.split(line, ",")

    [d, m, y] =
      String.split(date, "-")
      |> Enum.map(&int_parse/1)

    [hh, mm] =
      String.split(time, ":")
      |> Enum.map(&int_parse/1)

    %{
      :datetime => {{y, m, d}, {hh, mm, 00}},
      :location =>
        {Float.parse(loc1)
         |> elem(0),
         Float.parse(loc2)
         |> elem(0)},
      :pollutionLevel => int_parse(val)
    }
  end

  def findStations(readings) do
    readings
    |> Enum.map(fn x -> x[:location] end)
    |> Enum.uniq()
  end

  def nameStation({loc1, loc2}) do
    "station_#{loc1}_#{loc2}"
  end

  def flushStations(coordsToName) do
    for {coords, name} <- coordsToName do
      :pollution_gen_server.addStation(name, coords)
    end
  end

  def flushMeasurements(readings) do
    for reading <- readings do
      :pollution_gen_server.addValue(
        reading[:location],
        reading[:datetime],
        "PM10",
        reading[:pollutionLevel]
      )
    end
  end
end
