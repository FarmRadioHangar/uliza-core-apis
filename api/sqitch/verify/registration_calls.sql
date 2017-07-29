BEGIN;

  SELECT id, phone_number, schedule_time, created_at
    FROM farmradio_api.registration_calls
  WHERE FALSE;

ROLLBACK;
