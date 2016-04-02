
create function set_revision_number()
returns trigger as $$
declare last_revision_number int;
begin
  select coalesce(max(er.revision_number), 0) into last_revision_number
    from exercise_revisions er where er.exercise_id = NEW.exercise_id;

  NEW.revision_number = last_revision_number + 1;

  return NEW;
end;
$$ language plpgsql;

create trigger set_revision_number before insert on exercise_revisions
  for each row execute procedure set_revision_number();
