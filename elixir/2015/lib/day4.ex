defmodule Day4 do
  @spec process(String.t(), String.t(), integer()) :: integer()
  defp process(prefix, secret, num) do
    result = :crypto.hash(:md5, "#{secret}#{num}") |> Base.encode16()

    if String.starts_with?(result, prefix) do
      num
    else
      process(prefix, secret, num + 1)
    end
  end

  @spec main() :: nil
  def main() do
    secret =
      File.read!("../../inputs/2015/Day4/input.txt") |> String.trim_trailing("\n")

    IO.inspect("Secret: #{secret}")

    result = process("00000", secret, 1)
    IO.inspect(result)

    result = process("000000", secret, 1)
    IO.inspect(result)
  end
end
