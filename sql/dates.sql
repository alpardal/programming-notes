
-- three days ago
select now() - '3 days'::interval;

-- begin of the month
select date_trunc('month', now());

-- all days of current month:
select
  generate_series(date_trunc('month', now()),
     date_trunc('month', now() + '1 month'::interval) - '1 day'::interval,
     '1 day'::interval);
