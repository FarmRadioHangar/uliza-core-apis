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
    registration_call_id            INT                 NULL               REFERENCES farmradio_api.registration_calls(id),
    created_at                      TIMESTAMPTZ         DEFAULT NOW() 
  );

  CREATE UNIQUE INDEX participants_phone_number_idx ON farmradio_api.participants (phone_number);
  CREATE INDEX participants_registration_call_idx ON farmradio_api.participants (registration_call_id);

  GRANT SELECT ON farmradio_api.participants TO www;

  GRANT ALL ON farmradio_api.participants TO api_consumer;
  GRANT USAGE, SELECT ON SEQUENCE farmradio_api.participants_id_seq TO api_consumer;

COMMIT;
