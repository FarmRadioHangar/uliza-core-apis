BEGIN;

	SELECT pg_catalog.pg_has_role('admin', 'usage');

ROLLBACK;
