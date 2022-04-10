defmodule SchemaDump do
  # override the following aliases in your mix.exs file:
  defp aliases do
    [
      "ecto.setup": ["ecto.create", "ecto.load", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.migrate": ["ecto.migrate", "ecto.dump"],
    ]
  end
end
