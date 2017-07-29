BEGIN;

	SELECT pg_catalog.pg_has_role('api_consumer', 'usage');

ROLLBACK;
