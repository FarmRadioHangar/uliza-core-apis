BEGIN;

  SELECT id, phone_number, registration_status, registration_call_id, created_at
    FROM farmradio_api.participants
  WHERE FALSE;

ROLLBACK;
