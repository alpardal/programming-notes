defmodule Ecto.RepoAndQueryExamples do
  # https://hexdocs.pm/ecto/Ecto.Query.API.html

  def loading_records do
    Repo.get(User, user_id)
    Repo.get_by(User, email: "john@example.com")
    # raise on missing record
    Repo.get!(User, user_id)
  end

  def select do
    # struct, w/ all columns loaded:
    Repo.all(User)
    # same as:
    Repo.all(from(u in User))
    # or:
    Repo.all(from(u in User, select: u))
    # bindings not need with a single `from`:
    Repo.one from User, limit: 1
    # returns list of User structs, w/ only the email populated
    Repo.all(from(User, select: [:email]))
    # returns a list of maps w/ single email key:
    Repo.all(from("users", select: [:email]))
    # returns a list of emails
    Repo.all(from(u in User, select: u.email))
    # same as
    Repo.all(from(u in "users", select: u.email))
    # returns a list of maps w/ key :email
    Repo.all(from(u in User, select: %{email: u.email}))
  end

  def where do
    # filtering by id:
    Repo.all(from(u in User, select: u.email, where: u.id == 1))
    # same as:
    Repo.all(from(u in User, select: u.email, where: [id: 1]))
    # filter by variable:
    id = 1
    Repo.all(from(u in User, select: u.email, where: [id: ^id]))
    # conditions
    Repo.all(from(u in User, select: u.email, where: u.id > 2))
    # OR:
    Repo.all(from(u in User, select: u.email, where: u.id == 2 or u.id == 3))
    # dynamic conditions:
    conds = [role: "Admin", some_other_cond: some_value]
    Repo.all(from(u in User, where: ^conds))
    # another example:
    conds =
      if params["created_after"] do
        dynamic([u], u.inserted_at >= params["created_after"])
      else
        true
      end

    Repo.all(from(u in User, where: ^conds))
  end

  def like do
    Repo.all(from(u in User, where(ilike(u.name, "j%"))))
  end

  def order do
    query = from(u in User, select: u.email, order_by: u.email)
    # same as:
    query = from(u in User, select: u.email, order_by: [asc: u.email])
    Repo.all(query)
    # dynamic:
    order = [desc: :email]
    Repo.all(from(u in User, order_by: ^order))
    # taking nulls into account:
    Repo.all(from(User, order_by: [asc_nulls_first: :age]))
  end

  def limit do
    # returns list of 1 item
    Repo.all(from(u in User, limit: 1))
    # returns the item directly or nil
    Repo.one(from(u in User))
    # throws if item is not found
    Repo.one!(from(u in User, where: [id: 1]))
  end

  def update do
    # using changesets:
    user = Repo.get!(User, user_id)
    changes = Ecto.Changeset.change(user, email: "new email")
    Repo.update(changes)
    # bulk update using query:
    query = from("users", where: [id: ^user_id])
    Repo.update_all(query, set: [email: ^new_email])
    # inc value by given amount:
    # use -1 to dec
    Repo.update_all(query, inc: [some_value: 1])
    # add value to array column:
    Repo.update_all(query, update: [push: [tags: "cool"]])
    # remove value from array column:
    Repo.update_all(query, update: [pull: [tags: "not cool"]])
  end

  def inserts do
    # bulk insert
    Repo.insert_all(User, [
      [name: "John", email: "john@example.com"],
      [name: "Paul", email: "paul@example.com"]
    ])

    # also works w/ structs:
    Repo.insert_all(User, [%{name: "John", email: "john@example.com"}])
    # choosing which struct fields will be loaded: (postgres only)
    Repo.insert_all(User, [%{name: "Paul"}], returning: [:id])
    # using the table name - returns %{id: <some-id>}
    Repo.insert_all("users", [%{name: "Paul"}], returning: [:id])
    # using schemas:
    {:ok, _new_record} = Repo.insert(%User{name: "Thomas"})
    # associations can be created in one go as well:
    Repo.insert(%User{name: "Thomas", profile: %User.Profile{...}})
    # using changesets
    attrs = %{name: "John"}
    Repo.insert(%User{} |> Ecto.Changeset.cast(attrs, [:name]))
  end

  def upserts do
    Repo.insert_all(
      User,
      [%{id: 1, name: "Paul"}, %{id: 2, name: "John"}],
      returning: [:id],
      on_conflict: {:replace, [:name]},
      conflict_target: [:id]
    )

    # also valid in single updates:
    Repo.insert!(
      %User{name: "John"},
      # name value set directly:
      on_conflict: [set: [name: "John"]],
      conflict_target: :name
    )
  end

  def loading_association_data do
    # Get all comments for the given post
    Repo.all(assoc(post, :comments))
    # Or build a query on top of the associated comments
    query = from(c in assoc(post, :comments), where: not is_nil(c.title))
    Repo.all(query)
  end

  def delete do
    query = from("users", where: [id: 3])
    Repo.delete_all(query)
    # using schemas:
    user = Repo.get(User, id: 1)
    Repo.delete(user)
  end

  def pipe_syntax do
    query =
      User
      |> select([u], u.email)
      |> where([u], u.id == 1)
      |> order_by(asc: :email)

    Repo.all(query)
  end

  def joins do
    Repo.all(
      from(u in User,
        join: r in assoc(u, :role),
        where: r.name == "Admin"
      )
    )
    # same as:
    Repo.all(
      from(u in User,
        join: r in Role,
        on: u.role_id == r.id,
        where: r.name == "Admin"
      )
    )
    # other types of joins available are:
    #   `left_join`, `right_join`, `cross_join`, `full_join`
  end

  def fragments do
    # ex1:
    name = "John"

    Repo.all(
      from(u in User,
        where:
          fragment(
            "lower(username) = ?",
            ^String.downcase(name)
          )
      )
    )

    # ex2:
    query =
      from(t in Track,
        join: a in Album,
        on: t.album_id == a.id,
        select: [a.title, fragment("array_agg(?)", t.title)],
        group_by: [a.title]
      )

    Repo.all(query)
  end

  def preloading do
    users_with_profiles = from(User, preload: :profile)
    Repo.one(from(users_with_profiles, limit: 1))
    user_id = 1
    Repo.get(users_with_profiles, user_id)
    # same as:
    Repo.get(User, user_id) |> Repo.preload(:profile)

    # preleoad in a single query:
    Repo.all(from(u in User, join: p in assoc(u, :profile), preload: :profile))
  end

  def aggregates do
    Repo.aggregate(User, :avg, :age)
    # gets translated into:
    Repo.one(from(u in User, select: avg(u.age)))
    # count:
    Repo.aggregate(User, :count)
    # same as:
    Repo.aggregate(User, :count, :id)
    # or:
    Repo.one(from(u in User, select: count(u.id)))
    # and:
    Repo.aggregate("users", :count, :id)

    # limiting:
    # tries to get the age average for the last 5 users to register,
    # but won't work: the limit is applied to the avg query,
    # which already has only one result: the avg
    Repo.one(from(u in User, order_by: [created_at: :desc], limit: 5, select: avg(u.age)))
    # the correct way is:
    latest_users = from(u in User, order_by: [created_at: :desc], limit: 5)
    Repo.aggregate(latest_users, :avg, :age)
    # which is equivalent to:
    Repo.one(
      from(u in subquery(from(u in User, order_by: ..., limit: 5)),
        select: avg(u.age)
      )
    )
  end

  def subqueries do
    Repo.one(from(u in subquery(from(_ in User, order_by: [desc: :id], limit: 1)), select: u.id))
    # same as:
    last_user_query = from(_ in User, order_by: [desc: :id], limit: 1)
    Repo.one(from(u in subquery(last_user_query), select: u.id))
  end

  def raw_sql do
    Repo.query("select * from users limit 1")
    # or `Repo.query!`
  end

  def to_sql do
    query = from(u in User, select: u.name)
    Ecto.Adapters.SQL.to_sql(:all, Repo, query)
    # same as:
    Repo.to_sql(:all, query)
  end

  def type_casting do
    # not an int
    user_id = "1"
    Repo.one(from("users", where: [id: type(^user_id, :integer)]))
    # not needed when using schemas:
    Repo.one(from(u in User, where: u.id == user_id))
  end

  def nulls do
    Repo.all(from(u in User, where: is_nil(u.age)))
    # vs
    Repo.all(from(u in User, where: not is_nil(u.age)))
  end

  def dynamic_fields do
    field = :email
    Repo.all(from(u in User, where: field(u, ^field) == "some-value"))
    # select * from users where email = 'some-value';
  end

  def composing_queries do
    miles_albums =
      from(al in Album,
        join: ar in Artist,
        on: ar.id == al.artist_id,
        where: ar.name == "Miles Davis"
      )

    miles_tracks = from([al, _ar] in miles_albums, join: tr in Track, on: tr.album_id == al.id)
    # since the second binding is not used, it could be omited:
    miles_tracks2 = from(al in miles_albums, join: tr in Track, on: tr.album_id == al.id)
    # binding order is preserved, but aliases are meaningless:
    Repo.all(from([_al, _ar, track] in miles_tracks, select: track.title))

    # using named bindings:
    temps = from Measurement, as: :temp, where: [type: "temperature"]
    Repo.one from [temp: t] in temps, where: t.value == 12.8, limit: 1
    # named bindings in joins:
    miles_albums3 =
      from(a in Album,
        as: :albums,
        join: ar in Artist,
        as: :artists,
        on: ar.id == a.artist_id,
        where: ar.name == "Miles Davis"
      )

    miles_tracks3 =
      from([albums: a] in miles_albums3, join: t in Track, as: :tracks, on: t.album_id == a.id)

    Repo.all(from([tracks: t] in miles_tracks3, select: t.title))
    # checking for named binding:
    Ecto.Query.has_named_binding?(miles_tracks3, :albums)

    # late binding with `as`:
    query = from m in Measurement, join: d in assoc(m, :device), as: :device
    Repo.all from m in query, where: m.value == 12.8, where: as(:device).id == 1

    # or_where:
    miles_or_evans_albums =
      from([a, ar] in miles_albums,
        or_where: ar.name == "Bill Evans",
        select: %{album: a.title, artist: ar.name}
      )

    Repo.all(miles_or_evans_albums)
  end

  def transactions do
    Repo.transaction do
      # usually you'll rely on Ecto.Multi, but if you run manual db
      # changes here, be sure to only run operations that raise on
      # failure so the transaction actually gets rolled back
      Repo.rollback("reason")
      # ^ manually rolling back if needed - the return value will be:
      #     {:error, "reason"}
    end
  end
end
