BEGIN;

  CREATE TYPE registration_event_type AS ENUM (
    'REGISTRATION_CALL_SCHEDULED', 
    'REGISTRATION_DECLINED', 
    'REGISTRATION_COMPLETE'
  );

  CREATE TABLE farmradio_api.participant_registration_status_log (
    id                   SERIAL                  PRIMARY KEY,
    participant_id       INT                     NOT NULL,
    event_type           registration_event_type NOT NULL,
    created_at           TIMESTAMPTZ             DEFAULT NOW() 
  );

  GRANT SELECT ON farmradio_api.participant_registration_status_log TO www;

  GRANT ALL ON farmradio_api.participant_registration_status_log TO api_consumer;
  GRANT USAGE, SELECT ON SEQUENCE farmradio_api.participant_registration_status_log_id_seq TO api_consumer;

COMMIT;
