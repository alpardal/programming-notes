
begin;

  create table colors (
    id int generated always as identity,
    name varchar not null
  );

  insert into colors (name) values ('red'), ('green'), ('blue');

  select * from colors;

  prepare get_color as
  select * from colors where name = $1;

  execute get_color('red');

rollback;
