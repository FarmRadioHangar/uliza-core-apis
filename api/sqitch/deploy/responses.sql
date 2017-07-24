BEGIN;

  CREATE TABLE farmradio_api.responses (
    id   SERIAL PRIMARY KEY,
    data JSONB
  );

  GRANT SELECT ON farmradio_api.responses TO www;

  GRANT ALL ON farmradio_api.responses TO admin;
  GRANT USAGE, SELECT ON SEQUENCE farmradio_api.responses_id_seq TO admin;

COMMIT;
