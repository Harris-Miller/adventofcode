defmodule Day1 do
  @spec parse(charlist()) :: list(integer())
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

  @spec parse(String.t()) :: list(integer())
  def parse(s), do: String.to_charlist(s) |> parse()

  @spec findPosition({integer(), integer()}) :: integer()
  def findPosition([{v, i} | tail]) do
    if v == -1 do
      i
    else
      findPosition(tail)
    end
  end

  @spec main() :: nil
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
