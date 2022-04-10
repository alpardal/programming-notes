# creating a mix projet:

# run `mix new app-name`
#
# OR:

# 1. Define you module e.g. in `lib/some_app/main.ex`:

defmodule SomeApp.Main do
  def start(:normal, []) do
    IO.puts "this is the project starting point."
    {:ok, self()}
  end
end

# 2. Define your `mix.exs` file:

defmodule SomeApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :some_app,
      version: "0.1.0"
    ]
  end

  def application do
    [
      mod: {SomeApp.Main, []}
    ]
  end
end

# 3. Run the project using `mix` or `mix run`
#
# ###################################################
#
# creating an executable w/ escript:
#
#
# add the `escript` option to your project entry in mix.exs:
#
def project do
  [
    app: :your_app,
    escript: [ main_module: YourApp.TheMainModule ],
    ...
  ]
end

# `YourApp.TheMainModule` must define a `main` function that
# accepts the list of args
