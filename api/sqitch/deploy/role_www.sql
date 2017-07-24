BEGIN;

  CREATE ROLE www NOLOGIN;

  GRANT www TO postgres;

COMMIT;
