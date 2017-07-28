BEGIN;

  CREATE TYPE registration_status AS ENUM (
    'NOT_REGISTERED', 
    'CALL_SCHEDULED', 
    'DECLINED', 
    'REGISTERED'
  );

  CREATE TABLE farmradio_api.participants (
    id                  SERIAL              PRIMARY KEY,
    phone_number        VARCHAR             NOT NULL,
    registration_status registration_status NOT NULL
  );

  GRANT SELECT ON farmradio_api.participants TO www;

  GRANT ALL ON farmradio_api.participants TO admin;
  GRANT USAGE, SELECT ON SEQUENCE farmradio_api.participants_id_seq TO admin;

COMMIT;
