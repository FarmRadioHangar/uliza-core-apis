-- Verify farmradio_api:api_schema on pg

BEGIN;

	SELECT pg_catalog.has_schema_privilege('farmradio_api', 'usage');

ROLLBACK;
