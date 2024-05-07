defmodule Advent2022 do
  use Application

  @impl true
  def start(_type, _args) do
    Day5.main()

    # Application.start just return a Task, so return it a noop
    Task.start(fn -> nil end)
  end
end
