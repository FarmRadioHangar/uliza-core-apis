BEGIN;

  CREATE SCHEMA farmradio_api;

  GRANT USAGE ON SCHEMA farmradio_api TO www;
  GRANT USAGE ON SCHEMA farmradio_api TO api_consumer;

COMMIT;
