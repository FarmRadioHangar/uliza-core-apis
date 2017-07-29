BEGIN;

  CREATE TYPE registration_event_type AS ENUM (
    'REGISTRATION_CALL_SCHEDULED', 
    'REGISTRATION_DECLINED', 
    'REGISTRATION_COMPLETE'
  );

  CREATE TABLE farmradio_api.participant_registration_status_log (
    id                   SERIAL                  PRIMARY KEY,
    participant_id       INT                     NOT NULL      REFERENCES farmradio_api.participants(id) ON DELETE CASCADE,
    registration_call_id INT                     NOT NULL      REFERENCES farmradio_api.registration_calls(id),
    event_type           registration_event_type NOT NULL,
    created_at           TIMESTAMPTZ             DEFAULT NOW() 
  );

  CREATE INDEX participant_registration_status_log_participant_idx ON farmradio_api.participant_registration_status_log (participant_id);
  CREATE INDEX participant_registration_status_log_registration_call_idx ON farmradio_api.participant_registration_status_log (registration_call_id);

  GRANT SELECT ON farmradio_api.participant_registration_status_log TO www;

  GRANT ALL ON farmradio_api.participant_registration_status_log TO api_consumer;
  GRANT USAGE, SELECT ON SEQUENCE farmradio_api.participant_registration_status_log_id_seq TO api_consumer;

COMMIT;
