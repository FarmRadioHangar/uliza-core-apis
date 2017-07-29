BEGIN;

  CREATE TABLE farmradio_api.registration_calls (
    id                    SERIAL              PRIMARY KEY,
    phone_number          VARCHAR             NOT NULL,
    schedule_time         TIMESTAMPTZ         NOT NULL,
    created_at            TIMESTAMPTZ         DEFAULT NOW() 
  );

  GRANT SELECT ON farmradio_api.registration_calls TO www;

  GRANT ALL ON farmradio_api.registration_calls TO api_consumer;
  GRANT USAGE, SELECT ON SEQUENCE farmradio_api.registration_calls_id_seq TO api_consumer;

COMMIT;
