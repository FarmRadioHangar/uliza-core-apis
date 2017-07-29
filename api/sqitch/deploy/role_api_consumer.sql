BEGIN;

  CREATE ROLE api_consumer NOLOGIN;

  GRANT api_consumer TO postgres;

COMMIT;
