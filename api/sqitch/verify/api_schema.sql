BEGIN;

	SELECT pg_catalog.has_schema_privilege('farmradio_api', 'usage');

ROLLBACK;
