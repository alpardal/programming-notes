defmodule Ecto.ChangesetExamples do
  import Ecto.Changeset

  # `change` is added to wrap data in a changeset, optionally adding changes
  # `put_change` adds a field change to the changeset
  def adding_changes do
    user = Repo.get(User, user_id)
    attrs = %{email: "new-email@example.com"})
    ch = change(user, attrs)
    IO.inspect ch.changes
    ch
    |> put_change(:field, "value")
    |> IO.inspect()
  end

  # casting converts column types and removes unlisted fields
  def casting do
    user = Repo.get(User, user_id)
    attrs = %{id: "1", ignored: "field"})
    ch = cast(user, attrs, [:id]) # converts id to integer
    ch.changes                    # :ignored won't appear here
    # setting which values should be considered empty:
    cast(user, Map.put(attrs, :field, "NULL"), [:id, :field],
         empty_values: ["", "NULL"])
  end

  def validations do
    ch = %User{}
         |> cast(params, [:name])
         |> validate_required([:name])
         |> validate_length(:name, min: 3)
    _errors = ch.errors # list errors
    # or traverse them:
    Ecto.Changeset.traverse_errors(ch, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)

    # custom validations:
    cast(user, %{age: 10}, [:age])
    |> validate_change(:id, fn field, value ->
      if value > 0, do: [], else: [{field, "must be positive"}]
    end)

    # or using validation functions:
    cast(user, %{age: 10}, [:age])
    |> validate_positive(:age)
  end

  defp validate_positive(changeset, field) do
    validate_change(changeset, field, fn _field, value ->
      if value > 0, do: [], else: [{field, "must be positive"}]
    end)
  end

  def updating_changes do
    field = :some_time_string_field
    update_change(ch, field, &Timex.parse!(&1, "{ISO:Extended}"))
  end

  def constraints do
    ch = cast(user, attrs, [:email])
         |> unique_constraint(:email)
    # constraints are enforced by the db, so errors will be empty:
    ch.errors # => []
    # only when trying to persist the change that errors will pop up:
    {:error, ch} = Repo.insert(ch)
    ch.errors # => [email: ...]
    # constraints short-circuit, while all validations are always checked.
    # unsafe validations (e.g. `unsafe_validate_unique`) provide a
    # workaround that's not immune to race conditions (the 'unsafe' part),
    # and should always be used alongside constraints.

    # Remember:
    #   "Using changeset constraints only makes sense if the error
    #    message is something the user can take action on"
    #  ^  since the checks are made by the db itself
  end

  def usage_without_schemas do
    form = %{artist_name: :string, album_title: :string}
    params = %{artist_name: "Charlie Parker", album_title: ""}

    ch =
      {%{}, form}
      |> cast(params, Map.keys(form))
      |> validate_required([:album_title])
    IO.inspect ch
  end
end
