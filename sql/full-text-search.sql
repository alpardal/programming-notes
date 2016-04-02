-- full-text search in Postgresql

-- search by title
select * from posts where to_tsvector(title) @@ to_tsquery('Mastering');

-- creating a search_field with trigger function:
create function add_search_field_to_post() returns trigger as $$
begin
  new.search_field :=
    setweight(to_tsvector('pg_catalog.simple', new.title), 'A') ||
    setweight(to_tsvector('pg_catalog.simple', new.body), 'B') ||
    setweight(to_tsvector('pg_catalog.simple',
                          array_to_string(new.tags, ' ', '')),
                          'C');

  return new;
end
$$ language plpgsql;

create trigger post_set_search_field_trigger
  before insert or update on posts for each row
  execute procedure add_search_field_to_post();
