
select '{"a": "b"}'::jsonb;
select '{"a":"b"}' || '{"c":"d"}'::jsonb;


-- create extension "uuid-ossp";

create table events (
  id uuid primary key default uuid_generate_v4(),
  attrs jsonb
);
create index on events using gin(attrs);

insert into events (attrs) values
  ('{"type": "new-post", "name": "some event" }'),
  ('{"type": "delete-post", "id": "some id"}'),
  ('{"type": "new-post", "name": "another event"}');

select attrs->>'type' as event_type from events;
select * from events where attrs ? 'id';
select * from events where attrs @> '{"type": "new-post"}';

select jsonb_agg(query) from (select * from events limit 1) query;

drop table events;
