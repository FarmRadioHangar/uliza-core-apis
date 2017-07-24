BEGIN;

  CREATE TABLE farmradio_api.countries (
    id           SERIAL       PRIMARY KEY,
    name         VARCHAR      NOT NULL,
    iso_2        VARCHAR(2)   NOT NULL,
    iso_3        VARCHAR(3)   NOT NULL,
    country_code VARCHAR(4)   NOT NULL
  );

  INSERT 
    INTO farmradio_api.countries (name, iso_2, iso_3, country_code)
    VALUES
      ('Burkina Faso', 'BF', 'BFA', '+226'),
      ('Ethiopia',     'ET', 'ETH', '+251'),
      ('Ghana',        'GH', 'GHA', '+233'),
      ('Malawi',       'MW', 'MWI', '+265'),
      ('Tanzania',     'TZ', 'TZA', '+255'),
      ('Uganda',       'UG', 'UGA', '+256'),
      ('Zambia',       'ZM', 'ZMB', '+260');

  GRANT SELECT ON farmradio_api.countries TO www;
      
COMMIT;
