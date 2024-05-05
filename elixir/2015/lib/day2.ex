defmodule Day2 do
  @spec determineAreaNeeded(list(integer())) :: integer()
  def determineAreaNeeded([l, w, h]) do
    sides = [l * w, l * w, l * h, l * h, w * h, w * h]
    Enum.sum(sides) + Enum.min(sides)
  end

  @spec determineLengthNeeded(list(integer())) :: integer()
  def determineLengthNeeded(list) do
    length =
      list |> Enum.sort() |> Enum.drop(-1) |> Enum.flat_map(fn x -> [x, x] end) |> Enum.sum()

    Enum.product(list) + length
  end

  @spec main() :: nil
  def main() do
    contents =
      File.read!("../../inputs/2015/Day2/input.txt")
      |> String.split("\n")
      # drop last empty line
      |> Enum.drop(-1)
      |> Enum.map(&(String.split(&1, "x") |> Enum.map(fn x -> String.to_integer(x) end)))

    part1 = Enum.map(contents, &Day2.determineAreaNeeded(&1)) |> Enum.sum()
    IO.inspect(part1)

    part2 = Enum.map(contents, &Day2.determineLengthNeeded(&1)) |> Enum.sum()
    IO.inspect(part2)
  end
end
