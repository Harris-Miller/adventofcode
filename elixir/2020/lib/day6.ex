defmodule Day6 do
  def main() do
    content =
      File.read!("../../inputs/2020/Day6/input.txt")
      |> String.trim_trailing("\n")
      |> String.split("\n\n")
      |> Enum.map(&String.split(&1, "\n"))

    part1 =
      content
      |> Enum.map(&Enum.reduce(&1, fn x, acc -> acc <> x end))
      |> Enum.map(&String.to_charlist(&1))
      |> Enum.map(&Enum.uniq(&1))
      |> Enum.map(&length(&1))
      |> Enum.sum()

    IO.inspect(part1)

    part2 =
      content
      |> Enum.map(fn xs ->
        Enum.map(xs, &String.split(&1, "", trim: true))
        |> Enum.map(&MapSet.new(&1))
        |> Enum.reduce(&MapSet.intersection/2)
      end)
      |> Enum.map(&MapSet.size/1)
      |> Enum.sum()

    IO.inspect(part2)
  end
end
