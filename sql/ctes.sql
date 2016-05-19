
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
