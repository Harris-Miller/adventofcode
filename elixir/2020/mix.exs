defmodule Advent2020.MixProject do
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
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Advent2020, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:witchcraft, "~> 1.0"}
    ]
  end
end
