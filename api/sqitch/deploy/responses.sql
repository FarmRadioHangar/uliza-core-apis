BEGIN;

  CREATE TABLE farmradio_api.responses (
    id   SERIAL PRIMARY KEY,
    data JSONB
  );

  GRANT SELECT ON farmradio_api.responses TO www;

COMMIT;
