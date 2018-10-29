with
  t1(v1, v2) as (select 1, 2),
  t2(v3, v4) as (
    select v1*2, v2*2 from t1
  )
  select * from t1, t2;

with recursive t(n) as (
  select 1 union all
    select n+1 from t
)
select n from t limit 10;

with recursive fib(a, b) as (
  select 0, 1 union all
    select b, a+b from fib
)
select a from fib limit 12;

begin;
  create table ids (value int);
  insert into ids (select * from generate_series(1, 6));

  with recursive vs as (
    select (i).*, pg_try_advisory_lock((i).value) as locked
    from (
      select i from ids as i where mod(value, 2) = 0 order by value limit 1
    ) as t1
    union all (
      select (i).*, pg_try_advisory_lock((i).value) as locked
      from (
        select (select i from ids as i where value > vs.value order by value limit 1) as i
               from vs limit 1
      ) as t1
    )
  )
  select *
  from vs
  where locked
  limit 1;

rollback;
