defmodule Day3 do
  require Integer

  @spec move(char(), {integer(), integer()}) :: {integer(), integer()}
  def move(c, {x, y}) do
    case c do
      ?^ -> {x, y + 1}
      ?> -> {x + 1, y}
      ?v -> {x, y - 1}
      ?< -> {x - 1, y}
      _ -> {x, y}
    end
  end

  @spec main() :: any()
  def main() do
    contents =
      File.read!("../../inputs/2015/Day3/input.txt")

    parsed =
      to_charlist(contents)
      |> Enum.scan({0, 0}, &Day3.move(&1, &2))

    parsed = [{0, 0}] ++ parsed

    # part 1
    r1 = Enum.group_by(parsed, & &1) |> Map.keys() |> length()
    IO.inspect(r1)

    # part 2
    {santa, robot} =
      contents
      |> to_charlist()
      |> Enum.with_index()
      |> Enum.split_with(fn {_, i} -> Integer.is_even(i) end)

    santa =
      ([{0, 0}] ++ santa)
      |> Enum.map(&elem(&1, 0))
      |> Enum.scan({0, 0}, &Day3.move(&1, &2))

    robot =
      ([{0, 0}] ++ robot)
      |> Enum.map(&elem(&1, 0))
      |> Enum.scan({0, 0}, &Day3.move(&1, &2))

    r2 = Enum.group_by(santa ++ robot, & &1) |> Map.keys() |> length()
    IO.inspect(r2)
  end
end
