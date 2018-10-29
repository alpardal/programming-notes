-- source: https://www.citusdata.com/blog/2018/02/22/seven-tips-for-dealing-with-postgres-locks/

-- #############################
-- # Tips for dealing w/ locks #
-- #############################


--  1: Never add a column with a default value

  -- instead of this:

    -- reads and writes block until it is fully rewritten (hours?)
      ALTER TABLE items ADD COLUMN last_update timestamptz DEFAULT now();

  -- do:

    -- select, update, insert, and delete block until the catalog is update (milliseconds)
    ALTER TABLE items ADD COLUMN last_update timestamptz;
    -- select and insert go through, some updates and deletes block while the table is rewritten
    UPDATE items SET last_update = now();

    -- Or better yet, avoid blocking updates and delete for a long time by updating in small batches, e.g.:

    -- do {
    --   numRowsUpdated = executeUpdate(
    --     "UPDATE items SET last_update = ? " +
    --     "WHERE ctid IN (SELECT ctid FROM items WHERE last_update IS NULL LIMIT 5000)",
    --     now);
    -- } while (numRowsUpdate > 0);


-- 2: Beware of lock queues, use lock timeouts


  -- When you can have long-running SELECT queries on a table, don’t do this:

    ALTER TABLE items ADD COLUMN last_update timestamptz;

  -- instead, do this:

    SET lock_timeout TO '2s'
    ALTER TABLE items ADD COLUMN last_update timestamptz;

-- 3: Always create indexes CONCURRENTLY

  -- Creating an index concurrently does have a downside. If something goes wrong it does not roll back and leaves an unfinished ('invalid') index behind. If that happens, don’t worry, simply run DROP INDEX CONCURRENTLY items_value_idx and try to create it again.

-- 4: Take aggressive locks as late as possible


  -- For example, if you want to completely replace the contents of a table. Don’t do this:

    BEGIN;
    -- reads and writes blocked from here:
    TRUNCATE items;
    -- long-running operation:
    \COPY items FROM 'newdata.csv' WITH CSV
    COMMIT;

  -- Instead, load the data into a new table and then replace the old table:

    BEGIN;
    CREATE TABLE items_new (LIKE items INCLUDING ALL);
    -- long-running operation:
    \COPY items_new FROM 'newdata.csv' WITH CSV
    -- reads and writes blocked from here:
    DROP TABLE items;
    ALTER TABLE items_new RENAME TO items;
    COMMIT;

  -- There is one problem, we didn’t block writes from the start, and the old items table might have changed by the time we drop it. To prevent that, we can explicitly take a lock the table that blocks writes, but not reads:

    BEGIN;
    LOCK items IN EXCLUSIVE MODE;
    ...

-- 5: Adding a primary key with minimal locking

  -- Postgres makes it very easy to create a primary key using ALTER TABLE, but while the index for the primary key is being built, which can take a long time if the table is large, all queries will be blocked.

    ALTER TABLE items ADD PRIMARY KEY (id); -- blocks queries for a long time

  -- Fortunately, you can first do all the heavy lifting using CREATE UNIQUE INDEX CONCURRENTLY, and then use the unique index as a primary key, which is a fast operation.

    CREATE UNIQUE INDEX CONCURRENTLY items_pk ON items (id); -- takes a long time, but doesn’t block queries
    ALTER TABLE items ADD CONSTRAINT items_pk PRIMARY KEY USING INDEX items_pk;  -- blocks queries, but only very briefly

  -- By breaking down primary key creation into two steps, it has almost not impact on the user.

-- 6: Never VACUUM FULL

  -- VACUUM FULL rewrites the entire table to disk, which can take hours or days, and blocks all queries while doing it. While there some valid use cases for VACUUM FULL, such as a table that used to be big, but is now small and still takes up a lot of space, it is probably not your use case.


-- 7: Avoid deadlocks by ordering commands
