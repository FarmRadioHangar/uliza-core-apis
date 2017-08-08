exports.up = (pg) => {
  pg.sql("CREATE ROLE www NOLOGIN;");
  pg.sql("CREATE ROLE app NOLOGIN;");
  pg.sql("GRANT www TO postgres;");
  pg.sql("GRANT app TO postgres;");
  pg.sql("GRANT USAGE ON SCHEMA farmradio_api TO www;");
  pg.sql("GRANT USAGE ON SCHEMA farmradio_api TO app;");
};

exports.down = (pg) => {
  pg.sql("DROP ROLE www;");
  pg.sql("DROP ROLE app;");
};
