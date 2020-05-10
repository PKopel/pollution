defmodule PollutionLoader do
  def import_lines_from_CSV(fileName, size) do
    lines = File.read!(fileName)
            |> String.split("\n")
    if length(lines) > size do
      List.foldl(lines, [], fn(line, list) -> [parse_line(line)| list] end)
    else
      {:error, :less_than_size}
    end
  end

  def parse_date_time({result_map, [date, time | rest]}) do
    date_tuple = String.split(date, "-")
                 |> Enum.reverse()
                 |> List.to_tuple()
    time_tuple = String.split(hour, ":")
                 |> case do
                      [h, m] -> [h, m, 0]
                      other -> other
                    end
                 |> List.to_tuple()
    Map.put(result_map, :datetime, {date_tuple, time_tuple})
    {result_map, rest}
  end

  def parse_location({result_map, [x, y | rest]}) do
    Map.put(result_map, :location, {x, y})
    {result_map, rest}
  end

  def parse_value({result_map, [value]}) do
    Map.put(result_map, :pollutionLevel, {x, y})
    {result_map, rest}
  end

  def parse_line(line) when is_binary(line) do
    String.split(line, ",")
    |> Tuple.append({Map.new()})
    |> parse_date_time()
    |> parse_location()
    |> parse_value()
  end

  def identify_stations(measurement_list) do
    Lists.foldl(measurement_list, MapSet.new(), fn (m, acc) -> MapSet.put(acc, m.location)end)
    |> Map.to_list()
  end

  def create_stations(station_list) do
    :pollutonApp.start(0, 0)
    Lists.foldl(station_list, 0, fn ({x, y}, _acc) -> :pollutionApp.addStation('station_#{x}_#{y}', {x, y}) end)
  end

end
