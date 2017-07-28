BEGIN;

  SELECT id, participant_id, event_type, created_at
    FROM farmradio_api.participant_registration_status_log
  WHERE FALSE;

ROLLBACK;
