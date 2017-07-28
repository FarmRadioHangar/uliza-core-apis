BEGIN;

  SELECT id, data
    FROM farmradio_api.voto_response_data
  WHERE FALSE;

ROLLBACK;
