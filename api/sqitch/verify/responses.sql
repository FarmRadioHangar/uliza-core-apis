-- Verify farmradio_api:responses on pg

BEGIN;

  SELECT id, data 
    FROM farmradio_api.responses
  WHERE FALSE;

ROLLBACK;
