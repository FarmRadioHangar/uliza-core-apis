BEGIN;

  CREATE TYPE registration_status AS ENUM (
    'NOT_REGISTERED', 
    'REGISTERED',
    'DECLINED'
  );

  CREATE TABLE farmradio_api.participants (
    id                              SERIAL              PRIMARY KEY,
    phone_number                    VARCHAR             NOT NULL,
    registration_status             registration_status NOT NULL,
    registration_call_schedule_time TIMESTAMPTZ         NULL,
    created_at                      TIMESTAMPTZ         DEFAULT NOW() 
  );

  GRANT SELECT ON farmradio_api.participants TO www;

  GRANT ALL ON farmradio_api.participants TO api_consumer;
  GRANT USAGE, SELECT ON SEQUENCE farmradio_api.participants_id_seq TO api_consumer;

COMMIT;
