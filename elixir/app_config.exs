defmodule AppConfig do
  def example do
    # create config/config.exs and add:
    # (*.secret.exs files should be gitignored)

    import Config
    config :your_app_name, :a_key, "A value"

    import_config "#{Mix.env()}.exs"
    import_config "#{Mix.env()}.secret.exs"

    # these config/*.exs files contain *buildtime* config,
    # for runtime config see: https://hexdocs.pm/mix/1.9.4/Mix.Tasks.Release.html#module-runtime-configuration
  end
end
