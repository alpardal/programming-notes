
select array[1,2,3];
select '{1,2,3}'::numeric[];
select array[1,2,3] || 4;
select array[1,2,3] || array[4,5,6];
-- one-indexed:
select (array[1,2,3])[1];

create table users (
  id bigserial primary key,
  name text not null,
  age numeric not null,
  hobbies text[] not null default array[]::text[]
);
create index on users using gin(hobbies);

insert into users (name, age, hobbies) values
    ('Charlie', 18, '{guitar playing, movie watching}'),
    ('John', 32, '{singing, playing sports}'),
    ('Alice', 40, '{dancing, movie watching}'),
    ('Paul', 18, '{eating, going to the beach}'),
    ('Bob', 25, '{}');

select array_agg(distinct age) from users;
select * from users where hobbies @> array['movie watching'];

drop table users;
