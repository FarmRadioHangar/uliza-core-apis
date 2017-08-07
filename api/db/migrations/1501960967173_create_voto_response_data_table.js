exports.up = (pg) => {
  pg.createTable({
    schema: 'farmradio_api', 
    name: 'voto_response_data'
  }, {
    id: { 
      type: 'serial', 
      primaryKey: true 
    },
    data: {
      type: 'jsonb',
      notNull: true
    }
  });
  pg.sql("GRANT SELECT ON farmradio_api.voto_response_data TO www;");
};

exports.down = (pg) => {
  pg.dropTable({
    schema: 'farmradio_api',
    name: 'voto_response_data'
  });
};
