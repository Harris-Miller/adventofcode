defmodule AdventOfCode.MixProject do
  use Mix.Project

  def project do
    [
      # this always has to be set, even though I'm using `apps_paths`
      apps_path: "apps",
      apps_paths: %{advent2015: "2015", advent2020: "2020", advent2022: "2022"},
      version: "0.1.0",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Dependencies listed here are available only for this
  # project and cannot be accessed from applications inside
  # the apps folder.
  #
  # Run "mix help deps" for examples and options.
  defp deps do
    []
  end
end
