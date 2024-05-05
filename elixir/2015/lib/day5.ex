defmodule Day5 do
  @spec is_nice?(String.t()) :: boolean
  defp is_nice?(str) do
    vowels = ~r/[aeiou].*[aeiou].*[aeiou]/
    forbidden = ~r/ab|cd|pq|xy/
    double = ~r/(.)\1/
    !String.match?(str, forbidden) and String.match?(str, vowels) and String.match?(str, double)
  end

  # It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
  # It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
  @spec is_nice2?(String.t()) :: boolean
  defp is_nice2?(str) do
    double = ~r/(..).*\1/
    triple = ~r/(.).\1/
    String.match?(str, double) and String.match?(str, triple)
  end

  @spec main() :: nil
  def main() do
    content =
      File.read!("../../inputs/2015/Day5/input.txt")
      |> String.trim_trailing("\n")
      |> String.split("\n")

    # part 1
    IO.inspect(Enum.filter(content, &is_nice?/1) |> Enum.count())

    # part 2
    IO.inspect(Enum.filter(content, &is_nice2?/1) |> Enum.count())
  end
end
