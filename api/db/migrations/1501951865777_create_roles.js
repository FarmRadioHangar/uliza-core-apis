exports.up = (pg) => {
  pg.sql("CREATE ROLE www NOLOGIN;");
  pg.sql("CREATE ROLE api NOLOGIN;");
  pg.sql("GRANT www TO postgres;");
  pg.sql("GRANT USAGE ON SCHEMA farmradio_api TO www;");
};

exports.down = (pg) => {
};
