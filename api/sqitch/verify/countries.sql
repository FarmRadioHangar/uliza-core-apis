-- Verify farmradio_api:countries on pg

BEGIN;

  SELECT id, name, iso_2, iso_3, country_code
    FROM farmradio_api.countries
  WHERE FALSE;

ROLLBACK;
