exports.up = (pg) => {
  pg.sql('CREATE SCHEMA farmradio_api;');
};

exports.down = (pg) => {
  pg.sql('DROP SCHEMA farmradio_api CASCADE;');
};
