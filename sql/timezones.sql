begin;

  show timezone;
  set time zone 'America/Sao_Paulo';

  select (now() at time zone 'america/new_york');

rollback;
