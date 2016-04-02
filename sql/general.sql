-- distinct from:

    -- doesn't include users where birthplace_id is null;
    select * from users where birthplace_id <> 1;
    -- same as birthplace_id is null or birthplace_id <> 1:
    select * from users where birthplace_id is distinct from 1;

-- coalesce: selects first non-null value:
select first_name || coalesce(' ' || middle_initial || ' ', ' ') ||
        last_name as full_name from users;

-- casting

select cast (1 as float);
