begin;
  -- source: https://www.periscopedata.com/blog/4-ways-to-join-only-the-first-row-in-sql


  -- task: selecting users with their most recent widget

  -- setup:

  create table users (
    id serial primary key,
    name varchar
  );
  create table widgets (
    id serial primary key,
    name varchar,
    user_id integer references users (id)
  );
  insert into users (name) values ('John'), ('Paul');
  insert into widgets (name, user_id) values
  ('A widged', 1), ('Another one', 1), ('One for Paul', 2),
  ('Another for Paul', 2), ('Last one for John', 1);

  -- Approach #1: correlated subquerie (assumes indexed foreign key )

  --   "Correlated subqueries are subqueries that depend on the outer query.
  --    It’s like a for loop in SQL. The subquery will run once for each row
  --    in the outer query"

  select u.*, w.name as widget from users u join widgets w on w.id = (
    select id from widgets
    where widgets.user_id = u.id
    order by id desc
    limit 1
  );

  -- Approach #2: complete subquery

  --   "Correlated subqueries break down when the foreign key isn’t indexed,
  --    because each subquery will require a full table scan.
  --    In that case, we can speed things up by rewriting the query to use
  --    a single subquery, only scanning the widgets table once:"

  select u.*, most_recent_user_widget.name as widget from users u join (
    select distinct on (user_id) * from widgets
    order by user_id, id desc
  ) as most_recent_user_widget
  on u.id = most_recent_user_widget.user_id;

  --   "We’ve used Postgres’ DISTINCT ON syntax to easily query for only one
  --    widget per user_id. If your database doesn’t support something like
  --    DISTINCT ON, you have two options:"

  -- Approach #3: nested subqueries (if you have an ordered ID column)

  --   "In our example, the most recent row always has the highest id value.
  --    This means that even without DISTINCT ON, we can cheat with our nested
  --    subqueries like this:"

  select u.*, most_recent_user_widget.name as widget from users u join (
    select * from widgets
    where id in (
      select max(id) from widgets group by user_id
    )
  ) as most_recent_user_widget
  on u.id = most_recent_user_widget.user_id
  order by u.id;

  --   "We start by selecting the list of IDs representing the most recent
  --    widget per user. Then we filter the main widgets table to those IDs.
  --    This gets us the same result as DISTINCT ON since sorting by id and
  --    created_at happen to be equivalent."

  -- Approach #4: window functions (if you need more control)

  --   "If your table doesn’t have an id column, or you can’t depend on its min
  --    or max to be the most recent row, use row_number with a window function.
  --    It’s a little more complicated, but a lot more flexible:"

  select u.*, most_recent_user_widget.name as widget from users u join (
    select * from (
      select *, row_number() over (
        partition by user_id
        order by id desc
      ) as row_num
      from widgets
    ) as ordered_widgets
    where ordered_widgets.row_num = 1
  ) as most_recent_user_widget
  on u.id = most_recent_user_widget.user_id
  order by u.id;

  --  "'over (partition by user_id order by created_at desc' specifies a
  --   sub-table, called a window, per user_id, and sorts those windows by
  --   created_at desc. row_number() returns a row’s position within its window.
  --   Thus the first widget for each user_id will have row_number 1."

  --  "In the outer subquery, we select only the rows with a row_number of 1.
  --   With a similar query, you could get the 2nd or 3rd or 10th rows instead."

rollback;
