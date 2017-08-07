exports.up = (pg) => {
  pg.createTable({
    schema: 'farmradio_api',
    name: 'registration_calls'
  }, {
    id: { 
      type: 'serial', 
      primaryKey: true 
    },
    phone_number: {
      type: 'varchar',
      notNull: true
    },
    scheduled_time: {
      type: 'timestamptz',
      notNull: true
    },
    created_at: {
      type: 'timestamptz',
      notNull: true,
      default: pg.func('CURRENT_TIMESTAMP')
    }
  });
  pg.sql("GRANT SELECT ON farmradio_api.registration_calls TO www;");
  pg.sql("GRANT ALL ON farmradio_api.registration_calls TO app;");
  pg.sql("GRANT USAGE, SELECT ON SEQUENCE farmradio_api.registration_calls_id_seq TO app;");
};

exports.down = (pg) => {
  pg.dropTable({
    schema: 'farmradio_api',
    name: 'registration_calls'
  });
};
