-- Deploy farmradio_api:responses to pg
-- requires: api_schema

BEGIN;

  CREATE TABLE farmradio_api.responses (
    id   SERIAL PRIMARY KEY,
    data JSONB
  );

COMMIT;
