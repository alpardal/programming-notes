-- constraints:
CREATE TABLE products (
    price numeric NOT NULL CHECK (price > 0)
);

-- named constraints:
CREATE TABLE products (
    price numeric NOT NULL CONSTRAINT positive_price CHECK (price > 0)
);

-- table constraints:
CREATE TABLE products (
    price numeric,
    discounted_price numeric,
    CONSTRAINT valid_discount CHECK (price > discounted_price)
);

-- unique constraints:
-- (adding unique constraint automatically creates btree index on column(s))
create table products (
    product_no integer CONSTRAINT must_be_different UNIQUE,
    a integer,
    b integer,
    c integer UNIQUE,
    UNIQUE (a, b)
);

-- primary key:
create table products (id integer primary key);
-- same as:
create table products (id integer unique not null);
-- auto incrementing:
create table products (id serial);
-- same as (note absence of UNIQUE constraint):
create sequence products_id_seq;
create table products (id integer not null default nextval('products_id_seq'));
alter sequence products_id_seq owned by products.id;
-- better:
create table products (id serial primary key);

-- foreign keys:
create table products (id integer primary key);
create table orders (product_no integer references products (id));
-- or:
create table products (product_no integer primary key);
create table orders (product_no integer references products);
-- multiple columns:
create table t1 (
    a integer, b integer,
    foreign key (a, b) references other_table (c1, c2)
);
-- integrity:
create table order_items (
    product_no integer references products on delete restrict,
    order_id   integer references orders   on delete cascade
);
