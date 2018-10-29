
CREATE DOMAIN color_code AS text
  CHECK (VALUE ~ '^#?([a-f]|[A-F]|[0-9]){3}(([a-f]|[A-F]|[0-9]){3})?$');

CREATE TABLE menus (
  main_color color_code
);

insert into menus (main_color) values
  ('#fff');

select * from menus;

-- the inserts below will error out:

-- insert into menus (main_color) values
--   ('#fffr');

-- insert into menus (main_color) values
--   ('#ff');
