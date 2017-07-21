-- Revert farmradio_api:api_schema from pg

BEGIN;

  DROP SCHEMA farmradio_api;

COMMIT;
