defmodule Day1 do
  def main() do
    content =
      File.read!("../../inputs/2022/Day1/input.txt")
      |> String.trim_trailing("\n")
      |> String.split("\n\n")
      |> Enum.map(&String.split(&1, "\n"))

    sorted =
      content
      |> Stream.map(fn m ->
        m
        |> Stream.map(&Integer.parse(&1))
        |> Stream.map(&elem(&1, 0))
        |> Enum.sum()
      end)
      |> Enum.sort(:desc)

    part1 =
      sorted
      |> Enum.take(1)

    IO.inspect(part1)

    part2 =
      sorted
      |> Enum.take(3)
      |> Enum.sum()

    IO.inspect(part2)
  end
end
