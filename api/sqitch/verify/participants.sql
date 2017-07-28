BEGIN;

  SELECT id, phone_number, registration_status
    FROM farmradio_api.participants 
  WHERE FALSE;

ROLLBACK;
