defmodule Day1 do
  @spec parse(String.t() | charlist()) :: [-1 | 0 | 1]
  def parse([]), do: []
  def parse([head | tail]) do
    next =
      case head do
        ?( -> 1
        ?) -> -1
        _ -> 0
      end

    [next | parse(tail)]
  end
  def parse(s), do: s |> String.to_charlist() |> parse()

  @spec findPosition([{-1 | 0 | 1, integer()}]) :: integer()
  def findPosition([{v, i} | _]) when v == -1, do: i
  def findPosition([_ | tail]), do: findPosition(tail)

  @spec main() :: any()
  def main() do
    contents = File.read!("../../inputs/2015/Day1/input.txt")

    # part 1
    r1 = contents |> Day1.parse() |> Enum.sum()
    IO.inspect(r1)
    # part 2
    r2 =
      contents
      |> Day1.parse()
      |> Enum.scan(0, &(&1 + &2))
      |> Enum.with_index()
      |> Day1.findPosition()

    IO.inspect(r2 + 1)
  end
end
