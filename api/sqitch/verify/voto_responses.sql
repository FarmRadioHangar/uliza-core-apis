BEGIN;

  SELECT *
    FROM farmradio_api.voto_responses
  WHERE FALSE;

ROLLBACK;
