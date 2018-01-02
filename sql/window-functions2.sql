begin;

create table films (
  id serial primary key,
  release_year integer not null,
  category_id integer not null,
  rating numeric not null
);

insert into films (release_year, category_id, rating) values
  (2015, 1, 8.00), (2015, 2, 8.50), (2015, 3, 9.00),
  (2016, 2, 8.20), (2016, 1, 8.40), (2017, 2, 7.00);

-- averages using CTEs, without window functions:

with year_averages as (
  select release_year as year, avg(rating) as avg from films group by release_year
),
category_averages as (
  select category_id, avg(rating) as avg from films group by category_id
)
select f.id, f.release_year, f.category_id, f.rating,
       ya.avg as year_avg, ca.avg as category_avg
  from films f
  join year_averages ya on f.release_year = ya.year
  join category_averages ca on ca.category_id = f.category_id;

-- averages using window functions:

select id, release_year, category_id, rating,
  avg(rating) over (partition by release_year) as year_avg,
  avg(rating) over (partition by category_id) as category_avg
  from films;

-- finding year rank:

select id, release_year, category_id, rating,
       rank() over (partition by release_year order by rating desc) as year_rank
  from films order by id;

-- overall rank:
select id, release_year, category_id, rating,
       rank() over (order by rating desc) as overall_rank
  from films order by id;



rollback;
