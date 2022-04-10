defmodule Ecto.MultiExamples do
  alias Ecto.Multi

  def simple do
    user = %User{email: "user@example.com"}
    multi =
      Multi.new()
      |> Multi.insert(:user, user) # :user is an atom used to uniquely
                                   # identify the operation
      |> Multi.update(:another_op, ...)
    Repo.transaction(multi) # return value is a tuple of shape:
    # {:ok,
    #   %{
    #     user: %User{email: "user@example.com"},
    #     another_op: ...
    #   }
    # }
    #
    # or in case of errors:
    #
    # {:error, op_that_caused_the_error, invalid_changeset, changes_so_far}
    #
    # (where `changes_so_far` represents changes that actually got sent
    #  to the db, but were rolled back)
  end

  def custom_op do
    multi =
      Multi.new()
      |> Multi.run(:search, fn _repo, changes ->  # using run/3
        SearchEngine.update(changes[:artist])
      end)                                        # or run/5:
      |> Multi.run(:op, SomeModule, :some_function, ["custom args"])
    # Ecto will pass repo and changes as well - the custom args will follow
    Repo.transaction(multi)
  end
end
