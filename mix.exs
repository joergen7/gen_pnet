defmodule Combin.Mixfile do
  use Mix.Project

  def project do
    [app: :gen_pnet,
     version: "0.1.1",
     elixir: "~> 1.4.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     description: description(),
     package: package(),
     deps: deps()]
  end

  def application do
    []
  end

  defp deps do
    []
  end

  defp description do
    """
    A generic Petri net OTP library.
    """
  end

  defp package do
    [# These are the default files included in the package
     name: :gen_pnet,
     files: ["lib", "priv", "mix.exs", "README*", "readme*", "LICENSE*", "license*"],
     maintainers: ["Jorgen Brandt"],
     licenses: ["Apache 2.0"],
     links: %{"GitHub" => "https://github.com/joergen7/gen_pnet",
              "Docs"   => "https://cuneiform-lang.org/man/gen_pnet/index.html"}]
  end
end