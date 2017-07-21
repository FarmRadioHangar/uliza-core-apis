-- Revert farmradio_api:countries from pg

BEGIN;

  DROP TABLE farmradio_api.countries;

COMMIT;
