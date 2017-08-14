var host = process.env.DB_HOST;
var user = process.env.DB_USER;
var pass = process.env.DB_PASS;
var name = process.env.DB_NAME;

var config = {
  database_url: 'postgres://' + user + ':' + pass + '@' + host + '/' + name, 
  schema: 'farmradio_api',
  migrations_schema: 'public',
  migrations_table: 'farmradio_api_registration_middleware_test_migrations',
  dir: '../../../api/db/migrations'
};

module.exports = config;
