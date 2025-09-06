defmodule Lipex.MixProject do
  use Mix.Project

  def project do
    [
      app: :lipex,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      name: "Lipex",
      source_url: "https://github.com/yourusername/lipex",
      docs: [
        main: "Lipex",
        extras: ["README.md"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.27", only: :dev, runtime: false}
    ]
  end

  defp description do
    """
    Lipex - An Elixir-like Lisp syntax implementation using sequence literals.

    Provides a comprehensive Lisp-like syntax that closely resembles standard
    Elixir functionality, using the ~~(...) sequence literal syntax.
    """
  end

  defp package do
    [
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/yourusername/lipex"}
    ]
  end
end
