exports.up = (pg) => {
  pg.createTable({
    schema: 'farmradio_api', 
    name: 'countries'
  }, {
    id: { 
      type: 'serial', 
      primaryKey: true 
    },
    name: {
      type: 'varchar',
      notNull: true
    },
    iso_2: {
      type: 'varchar(2)',
      notNull: true
    },
    iso_3: {
      type: 'varchar(3)',
      notNull: true
    },
    country_code: {
      type: 'varchar(4)',
      notNull: true
    }
  });
  pg.sql("GRANT SELECT ON farmradio_api.countries TO www;");
  pg.sql("GRANT ALL ON farmradio_api.countries TO app;");
  pg.sql("GRANT USAGE, SELECT ON SEQUENCE farmradio_api.countries_id_seq TO app;");
  pg.sql(
    "INSERT INTO farmradio_api.countries \
      (name, iso_2, iso_3, country_code) \
     VALUES \
      ('Burkina Faso', 'BF', 'BFA', '+226'), \
      ('Ethiopia',     'ET', 'ETH', '+251'), \
      ('Ghana',        'GH', 'GHA', '+233'), \
      ('Malawi',       'MW', 'MWI', '+265'), \
      ('Tanzania',     'TZ', 'TZA', '+255'), \
      ('Uganda',       'UG', 'UGA', '+256'), \
      ('Zambia',       'ZM', 'ZMB', '+260'); \
    ");
};

exports.down = (pg) => {
  pg.dropTable({
    schema: 'farmradio_api',
    name: 'countries'
  });
};
