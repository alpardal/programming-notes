defmodule PreparedStatementsConfig do
  def on_your_postgres_db_config do
    * use PostgreSQL 12, if possible;
    * set plan_cache_mode: :force_custom_plan;
    * if you use PostgreSQL before version 12, set prepare: :unnamed;
    # more info: https://blog.soykaf.com/post/postgresql-elixir-troubles/
  end
end
