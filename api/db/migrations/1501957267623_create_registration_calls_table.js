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
};

exports.down = (pg) => {
  pg.dropTable({
    schema: 'farmradio_api',
    name: 'registration_calls'
  });
};
