-- casting

select cast (1 as float); -- SQL standard
select 1::real;

-- escaped string
select E'string \n with two lines';

-- hex and binary
select x'caf3';
select b'1101';

-- dollar-delimited string
select $$simple string$$;
select $tag$tagged string$tag$;

-- regexps
select * from books where title ~ 'Ruby';
select * from books where title ~* 'ruby';

-- series
select * from generate_series(1,10,2);
select * from generate_series('2014-01-01'::timestamp,
                              '2014-01-31'::timestamp, '3 days');

-- date math
select '1 week' + now();

-- timezones
select now() at time zone 'PDT' as california_time;
select now() - now() at time zone 'PDT';


-- case insensitive, LIKE indexing:
CREATE INDEX idx_table_lower_text ON table(lower(text_field));

SELECT * FROM table WHERE lower(text_field) LIKE 'xxxyy%';
