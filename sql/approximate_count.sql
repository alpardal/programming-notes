
-- postgres only:
SELECT reltuples as approx_count FROM pg_class WHERE relname = '<the-table-name>';
