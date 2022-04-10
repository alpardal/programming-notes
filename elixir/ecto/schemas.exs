defmodule Ecto.SchemaExamples do
  use Ecto.Schema
  alias MusicDB.{Artist, Track, Genre, AlbumGenre}

  schema "albums" do
    field(:title, :string)
    # array field
    field(:tags, {:array, :string}, default: [])
    # field from another column:
    field(:author, :string, source: :user)
    # virtual fields
    field(:password, :string, virtual: true)
    # should be read from the db (e.g. autoincremented values)
    field(:global_position, :integer, read_after_writes: true)
    timestamps()

    belongs_to(:artist, Artist)
    has_many(:tracks, Track)
    many_to_many(:genres, Genre, join_through: AlbumGenre)
    # same as, but w/out having to setup the AlbumGenre schema:
    many_to_many(:genres, Genre, join_through: "albums_genres")
    # (^ in this case, the table cannot have fields other than the fks)
  end

  def working_with_associations do
    # building new associated records:
    album = Repo.get(Album, 1)
    _new_track = Ecto.build_assoc(album, :tracks, title: "A Song")

    # `put_assoc` deals w/ the entire collection, so you have to be
    # explicity about how you want existing records to be handled:
    album
    |> change
    |> put_assoc(:tracks, [%Track{title: "Another Song"}])

    # ^ this will blow up unless the album doesn't have any tracks yet
    # or the association is configured w/ the proper :on_replace option, e.g.:
    has_many(:tracks, Track, on_replace: :nilify)
    # (see https://hexdocs.pm/ecto/Ecto.Schema.html#has_many/3-options)

    # `cast_assoc` is meant to be used w/ external params, that'll have
    # to be cast, validated, etc:
    params = %{title: "Some Album", tracks: [%{title: "First Track"}]}

    ch =
      %Album{}
      |> cast(params, [:title])
      |> cast_assoc(:tracks)
      # ^ the Track module is expected to implement a `changeset/2` function
      # that will be used to generated the changeset for each track. You can
      # set another function to be used for that if needed:
      |> cast_assoc(:tracks, with: &SomeModule.some_function/2)
  end
end
