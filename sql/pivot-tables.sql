
-- nnoremap <leader>r :w\|!clear;echo;PGPASSWORD=secret_pass psql prevu_development --username postgres -h localhost -p 25434 -f crosstab.sql<cr>

begin;

  create view areas_counties as (
    select county.name as county, area.name, area.area_type from geo_areas area join geo_areas county on county.id = area.county_id
  );

  -- select county, type, count(name) from areas_counties where type in
  --   ('macro-neighborhood', 'sub-neighborhood', 'neighborhood') group by 1, 2;

  select * from crosstab(
    $$select county, area_type, count(*) from areas_counties
        where area_type in ('macro-neighborhood', 'neighborhood', 'sub-neighborhood')
        group by 1, 2 order by 1, 2$$
  ) as result(
    "county" varchar, "macro-neighborhood" bigint, "neighborhood" bigint, "sub-neighborhood" bigint
  );

  -- -- psql shortcut:
  --
  -- select county, area_type, count(*) from areas_counties
  --   where area_type in ('neighborhood', 'sub-neighborhood', 'macro-neighborhood')
  --   group by 1, 2 order by 1, 2 \crosstabview

rollback;
