defmodule UsingUUIDsInPhoenix do
  def steps do
    configure_generators
    configure_migrations
    configure_schema
    # optional:
    create_own_app_schema
    # optional 2:
    create_uuids_on_db
  end

  defp configure_generators do
    # in config/config.exs:
    config :my_app,
      ecto_repos: [MyApp.Repo],
      generators: [binary_id: true]
  end

  defp configure_migrations do
    # when creating new tables set your own id column:
    create table(:users, primary_key: false) do
      add :id, :binary_id, primary_key: true
      timestamps()
    end
    # optionally, configure ecto to use uuids by default:
    config :app, App.Repo,
      migration_primary_key: [name: :uuid, type: :binary_id]
    # (see https://hexdocs.pm/ecto_sql/Ecto.Migration.html#module-repo-configuration)
  end

  defp configure_schema do
    # when changing schemas not generated after first change above,
    # change the primary key settings manually:

    @primary_key {:id, :binary_id, autogenerate: true}
    @foreign_key_type :binary_id

    schema "users" do
      # ...
    end
    # more info: https://hexdocs.pm/ecto/Ecto.Schema.html#module-schema-attributes
  end

  defp create_own_app_schema do
    # define your schema module and use instead of `Ecto.Schema`
    defmodule MyApp.Schema do
      defmacro __using__(_) do
        quote do
          use Ecto.Schema
          import Ecto.Changeset

          @primary_key {:id, :binary_id, autogenerate: true}
          @foreign_key_type :binary_id
        end
      end
    end
  end

  defp create_uuids_on_db do
    # e.g. using pgcrypto, change you migrations to:
    add :id, :binary_id, primary_key: true, default: fragment("gen_random_uuid()")
    # and update the schema definition:
    @primary_key {:id, :binary_id, read_after_writes: true}
  end
end
