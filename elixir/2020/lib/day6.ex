defmodule Day6 do
  def main() do
    content =
      File.read!("../../inputs/2020/Day6/input.txt")
      |> String.trim_trailing("\n")
      |> String.split("\n\n")
      |> Enum.map(&String.split(&1, "\n"))

    part1 =
      content
      |> Stream.map(fn xs ->
        xs
        |> Enum.reduce(&(&1 <> &2))
        |> String.to_charlist()
        |> Enum.uniq()
        |> length()
      end)
      |> Enum.sum()

    IO.inspect(part1)

    part2 =
      content
      |> Stream.map(fn xs ->
        xs
        |> Stream.map(&String.split(&1, "", trim: true))
        |> Stream.map(&MapSet.new/1)
        |> Enum.reduce(&MapSet.intersection/2)
      end)
      |> Stream.map(&MapSet.size/1)
      |> Enum.sum()

    IO.inspect(part2)
  end
end
