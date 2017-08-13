require('dotenv').config();

var assert   = require('assert');
var chai     = require('chai');
var client   = require('pg').Client;
var npgm     = require('node-pg-migrate');
var mocha    = require('mocha');
var request  = require('supertest');
var config   = require('./config');

var migrate  = npgm.default;
var unlock   = npgm.unlockRunner;
var postgres = new client(config.database_url);

postgres.connect();

chai.should();
chai.use(require('chai-things'));
chai.use(require('chai-also'));
chai.use(require('chai-http'));

function schemaUp() {
  console.log = function() {};
  return migrate(Object.assign({}, {
    direction: 'up'
  }, config))
  .then(function() {
    delete console.log;
  });
}

function schemaDown() {
  console.log = function() {};
  return migrate(Object.assign({}, {
    direction: 'down',
    count: Number.MAX_SAFE_INTEGER 
  }, config))
  .then(function() {
    delete console.log;
  });
}

module.exports = {
  schemaUp: schemaUp,
  schemaDown: schemaDown,
  unlock: function() { return unlock(config); },
  request: function() { return request(process.env.REG_SERVICE_URL); },
  db: postgres
};
