BEGIN;

  CREATE ROLE admin NOLOGIN;

  GRANT admin TO postgres;

COMMIT;
