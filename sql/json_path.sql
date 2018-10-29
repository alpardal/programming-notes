
-- available in Postgres 12+
-- docs: https://www.postgresql.org/docs/12/functions-json.html#FUNCTIONS-SQLJSON-PATH
--       https://www.postgresql.org/docs/12/datatype-json.html#DATATYPE-JSONPATH
-- presentation: http://filemirror.s3.amazonaws.com/jsonpath-pgday.it-2019.pdf

-- nnoremap <leader>r :w\|!clear;echo;psql -f %<cr>

begin;

  create table users (
    id int generated always as identity,
    profile jsonb
  );

  insert into users (profile) values
    ('{"name": "John", "address": {"street": "some street", "number": 32}, "array": [1,2,3]}'),
    ('{"name": "Paul", "address": {"street": "another street", "number": 20}, "optional": "value"}'),
    ('{"name": "Paul", "address": {"street": "another street", "number": 14}, "optional": null}');

   select * from users where jsonb_path_match(profile, '$.address.street == "some street"');
   select * from users where jsonb_path_match(profile, '$.address.number >= 20');
   select * from users where jsonb_path_exists(profile, '$.optional');
   select jsonb_path_query_array(profile, '$.array[1 to last]') from users;

rollback;
