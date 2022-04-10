defmodule MigrationSnippets do
  def table_creation do
    create table(:albums) do
      add :title, :string, null: false
      add :description, :text, null: false, default: ""
      add :year, :integer
      add :artist_id, references(:artists), null: false
      timestamps()  # adds `inserted_at` & `updated_at` cols
      # only add `inserted_at`:
      timestamps(updated_at: false)
    end
  end

  def table_altering do
    alter table(:videos) do
      # usual `create` function calls
    end
  end

  def indexes do
    create index(:albums, [:title, :year])  # atoms are 'interpreted' by ecto
    create index("albums", :year)           # strings are sent to db as-is
    create index(:bla, :col, unique: true)
    # same as:
    create unique_index(:bla, :col)
    # concurrently created indexes:
    #   - set module attrs:
    #     @disable_ddl_transaction true
    #     @disable_migration_lock true
    #   - and then:
    create index(:table, :col, concurrently: true)
  end

  def foreign_keys do
    # add fk w/out validating existing data:
    alter table(:posts) do
      add :group_id, references(:groups:, validate: false)
    end
    # enable validation in a later migration:
    execute "ALTER TABLE posts VALIDATE CONSTRAINT group_id_fkey", ""
  end

  # remember to first create the column w/out default values, populate
  # missing values, and only then set the default:
  def modifying_default_values do
    alter table(:comments) do
      modify :approved, :boolean, default: false
    end
  end

  def adding_constraints do
    # first, create the constraint w/out validating existing values:
    create constraint(:products, :positive_price, check: "price > 0"), validate: false
    # another example:
    create constraint(:products, :active_not_null, check: "active IS NOT NULL"), validate: false
    # then, in a later migration, enable validation after fixing existing issues:
    execute "ALTER TABLE products VALIDATE CONSTRAINT positive_price", ""
    # for the second example:
    execute "ALTER TABLE products VALIDATE CONSTRAINT active_not_null", ""
  end

  def custom_changes do
    alter table(:videos) do
      # make some changes...
    end

    flush() # so changes are actually sent to db before running upcoming code

    from(c in "compositions", select: [:id, :artist_id])
    |> Repo.all()
    |> # ... migrate data
  end

  def custom_pk do
    create table("some_table", primary_key: false) do
      add :code, :string, primary_key: true
      add :some_id, references("other_table", column: :code, type: :string)
    end
  end
end
