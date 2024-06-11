defmodule Day5 do
  @spec parseInt(String.t()) :: integer()
  defp parseInt(s), do: s |> Integer.parse() |> elem(0)

  @spec parse_move(String.t()) :: {integer(), integer(), integer()}
  defp parse_move(str) do
    str
    |> String.split(" ", trim: true)
    |> then(fn [_, x, _, from, _, to] -> {parseInt(x), parseInt(from), parseInt(to)} end)
  end

  # sample starting stack
  #     [D]
  # [N] [C]
  # [Z] [M] [P]
  #  1   2   3
  @stacks_samples [
    [?Z, ?N],
    [?M, ?C, ?D],
    [?P]
  ]

  # this is part of the input, it is the visualized stacks initial value
  #     [C]             [L]         [T]
  #     [V] [R] [M]     [T]         [B]
  #     [F] [G] [H] [Q] [Q]         [H]
  #     [W] [L] [P] [V] [M] [V]     [F]
  #     [P] [C] [W] [S] [Z] [B] [S] [P]
  # [G] [R] [M] [B] [F] [J] [S] [Z] [D]
  # [J] [L] [P] [F] [C] [H] [F] [J] [C]
  # [Z] [Q] [F] [L] [G] [W] [H] [F] [M]
  #  1   2   3   4   5   6   7   8   9
  @stacks_actual [
    [?Z, ?J, ?G],
    [?Q, ?L, ?R, ?P, ?W, ?F, ?V, ?C],
    [?F, ?P, ?M, ?C, ?L, ?G, ?R],
    [?L, ?F, ?B, ?W, ?P, ?H, ?M],
    [?G, ?C, ?F, ?S, ?V, ?Q],
    [?W, ?H, ?J, ?Z, ?M, ?Q, ?T, ?L],
    [?H, ?F, ?S, ?B, ?V],
    [?F, ?J, ?Z, ?S],
    [?M, ?C, ?D, ?P, ?F, ?H, ?B, ?T]
  ]

  defp view_stack(stacks, i) do
    Enum.at(stacks, i - 1)
  end

  defp set_stack(stacks, i, s) do
    List.replace_at(stacks, i - 1, s)
  end

  defp go(0, fromStack, toStack), do: {fromStack, toStack}

  defp go(n, fromStack, toStack) do
    {c, fromStack_} = List.pop_at(fromStack, -1)
    toStack_ = toStack ++ [c]
    go(n - 1, fromStack_, toStack_)
  end

  defp transfer(xs, {n, from, to}) do
    go(n, view_stack(xs, from), view_stack(xs, to))
  end

  defp move_crates({n, from, to}, stacks) do
    {fromStack, toStack} = transfer(stacks, {n, from, to})

    stacks
    |> set_stack(from, fromStack)
    |> set_stack(to, toStack)
  end

  defp go2(0, fromStack, temp), do: {fromStack, temp}

  defp go2(n, fromStack, temp) do
    {c, fromStack_} = List.pop_at(fromStack, -1)
    temp_ = [c | temp]
    go2(n - 1, fromStack_, temp_)
  end

  defp transfer2(xs, {n, from, to}) do
    go2(n, view_stack(xs, from), [])
  end

  defp move_crates2({n, from, to}, stacks) do
    {fromStack, temp} = transfer2(stacks, {n, from, to})
    toStack = view_stack(stacks, to) ++ temp

    stacks
    |> set_stack(from, fromStack)
    |> set_stack(to, toStack)
  end

  def main() do
    content =
      File.read!("../../inputs/2022/Day5/input.txt")
      |> String.trim_trailing("\n")
      |> String.split("\n")
      |> Enum.map(&parse_move/1)

    stacks = @stacks_actual

    part1 =
      Enum.reduce(content, stacks, &move_crates/2)
      |> Enum.map(&List.last/1)

    IO.inspect(part1)

    part2 =
      Enum.reduce(content, stacks, &move_crates2/2)
      |> Enum.map(&List.last/1)

    IO.inspect(part2)
  end
end
