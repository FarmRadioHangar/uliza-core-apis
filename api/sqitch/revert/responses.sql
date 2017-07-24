-- Revert farmradio_api:responses from pg

BEGIN;

  DROP TABLE farmradio_api.responses;

COMMIT;
