defmodule Day6 do
  defp parseNumPair(str) do
    str
    |> String.split(",")
    |> Enum.map(&Integer.parse/1)
    |> Enum.map(&elem(&1, 0))
    |> List.to_tuple()
  end

  @spec turnOn(list(String.t())) :: {:turnOn, {integer(), integer()}, {integer(), integer()}}
  defp turnOn(words) do
    from = Enum.at(words, 2) |> parseNumPair()
    to = Enum.at(words, 4) |> parseNumPair()
    {:turnOn, from, to}
  end

  @spec turnOff(list(String.t())) :: {:turnOff, {integer(), integer()}, {integer(), integer()}}
  defp turnOff(words) do
    from = Enum.at(words, 2) |> parseNumPair()
    to = Enum.at(words, 4) |> parseNumPair()
    {:turnOff, from, to}
  end

  @spec toggle(list(String.t())) :: {:toggle, {integer(), integer()}, {integer(), integer()}}
  defp toggle(words) do
    from = Enum.at(words, 1) |> parseNumPair()
    to = Enum.at(words, 3) |> parseNumPair()
    {:toggle, from, to}
  end

  @spec parse(String.t()) ::
          {:turnOn | :turnOff | :toggle, {integer(), integer()}, {integer(), integer()}}
  defp parse(str) do
    words = String.split(str, " ")

    case Enum.at(words, 1) do
      "on" -> turnOn(words)
      "off" -> turnOff(words)
      _ -> toggle(words)
    end
  end

  defp process({action, from, to}, grid) do
    func =
      case action do
        :turnOn -> fn _ -> :on end
        :turnOff -> fn _ -> :off end
        :toggle -> fn state -> if state == :on, do: :off, else: :on end
      end

    {y1, x1} = from
    {y2, x2} = to

    coords = for y <- y1..y2, x <- x1..x2, do: {y, x}

    Enum.reduce(coords, grid, fn {y, x}, acc ->
      Map.update!(acc, {y, x}, &func.(&1))
    end)
  end

  @spec main() :: nil
  def main() do
    grid = for y <- 0..999, x <- 0..999, into: %{}, do: {{y, x}, :off}

    content =
      File.read!("../../inputs/2015/Day6/input.txt")
      |> String.trim_trailing("\n")
      |> String.split("\n")

    instructions = content |> Enum.map(&parse/1)

    result =
      Enum.reduce(instructions, grid, &process/2) |> Map.values() |> Enum.count(&(&1 == :on))

    IO.inspect(result)
  end
end
