defmodule User do
  use Ecto.Schema
  @timestamps_opts [type: :utc_datetime_usec] # `_usec` to store microseconds

  schema "users" do
    field :name, :string
    timestamps()
    # or w/out module attr:
    # timestamps([type: :utc_datetime_usec])
  end
end

# migration:
#
#   def change do
#     create table(:users) do
#       add :name, :string
#
#       timestamps([type: :utc_datetime_usec])
#     end
#   end
#
# or set default type in `config.ex`:
#
#   config :your_app_name_here, YourAppNameHere.Repo,
#     migration_timestamps: [type: :utc_datetime_usec]
