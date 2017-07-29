BEGIN;

  CREATE TABLE farmradio_api.voto_response_data (
    id   SERIAL PRIMARY KEY,
    data JSONB  NOT NULL
  );

  GRANT SELECT ON farmradio_api.voto_response_data TO www;

  GRANT ALL ON farmradio_api.voto_response_data TO api_consumer;
  GRANT USAGE, SELECT ON SEQUENCE farmradio_api.voto_response_data_id_seq TO api_consumer;

COMMIT;
