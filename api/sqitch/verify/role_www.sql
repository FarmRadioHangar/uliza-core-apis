BEGIN;

	SELECT pg_catalog.pg_has_role('www', 'usage');

ROLLBACK;
