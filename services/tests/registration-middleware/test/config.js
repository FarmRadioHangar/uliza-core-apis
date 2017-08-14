var host = process.env.DB_HOST;
var user = process.env.DB_USER;
var pass = process.env.DB_PASS;

var config = {
  database_url: 'postgres://' + user + ':' + pass + '@' + host + '/postgres', // '/registration_middleware_test'conn,
  schema: 'farmradio_api',
  migrations_schema: 'public',
  migrations_table: 'farmradio_api_registration_middleware_test_migrations',
  dir: '../../../api/db/migrations'
};

module.exports = config;
