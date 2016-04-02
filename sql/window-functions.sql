
create table users(
  id serial primary key,
  name varchar(30) not null,
  age int not null
);

insert into users (name, age) values
  ('bob', 20), ('alice', 20), ('john', 35), ('camile', 19),
  ('charlie', 60), ('oswald', 40), ('alfred', 60), ('bruce', 19);


-- no frame clause:
select name, array_agg(name) over () from users;

-- all rows up to current row:
select name, array_agg(name) over (order by id) from users;
-- same as:
select name, array_agg(name) over (order by id rows between
  unbounded preceding and current row) from users;

-- all rows after current row:
select name, array_agg(name) over (order by id rows between
  current row and unbounded following) from users;

-- users with same age:
select name, age, array_agg(name) over (partition by age) from users;

-- prev and next user:
select id, name, lag(name, 1) over w as prev,
       lead(name, 1) over w as next
  from users window w as (order by id) order by id;


drop table users;
