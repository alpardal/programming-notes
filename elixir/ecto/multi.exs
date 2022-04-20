defmodule Ecto.MultiExamples do
  alias Ecto.Multi

  def simple do
    user_attrs = %User{email: "user@example.com"}
    # (usually, you will want to use changesets instead of raw attrs)

    multi =
      Multi.new()
      # :user is an atom used to uniquely identify the operation
      |> Multi.insert(:user, user_attrs)
      |> Multi.update(:another_op, op_params)

    # inspect the enqueued ops:
    IO.inspect(Multi.to_list(multi))

    # run the multi:
    Repo.transaction(multi)
    # ^^ return value is a tuple of shape:
    #   {:ok,
    #     %{
    #       user: %User{email: "user@example.com"},
    #       another_op: ...
    #     }
    #   }
    #
    # or in case of errors:
    #
    #   {:error, op_that_caused_the_error, invalid_changeset, changes_so_far}
    #
    # notice that the db will only get involved if all changesets are valid,
    # so in case of failed validations `changes_so_far` will be empty - it will
    # only be populated if we have changes that have been sent to the db but
    # got rolled back.
  end

  def custom_op do
    multi =
      Multi.new()
      # using run/3
      |> Multi.run(:search, fn _repo, changes ->
        SearchEngine.update(changes[:artist])
      end)

      # or run/5:
      |> Multi.run(:op, SomeModule, :some_function, ["custom args"])

    # ...which will call `SomeModule.some_function(repo, changes, *custom_args)`

    Repo.transaction(multi)
  end
end
