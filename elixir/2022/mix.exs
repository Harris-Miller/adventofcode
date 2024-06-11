defmodule Advent2022.MixProject do
  use Mix.Project

  def project do
    [
      app: :advent2015,
      version: "0.1.0",
      build_path: "../_build",
      config_path: "../config/config.exs",
      deps_path: "../deps",
      lockfile: "../mix.lock",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [flags: [:error_handling, :extra_return, :missing_return, :underspecs, :unknown, :unmatched_returns, :overspecs, :specdiffs]]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Advent2022, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false}
      # {:witchcraft, "~> 1.0"}
    ]
  end
end
