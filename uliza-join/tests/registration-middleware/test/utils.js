require('dotenv').config();

var mysql = require('mysql');
var glob  = require('glob');
var path  = require('path');

var DB_HOST = process.env.DB_HOST || '0.0.0.0';
var DB_PORT = process.env.DB_PORT || 3306;
var DB_USER = process.env.DB_USER || 'root';
var DB_NAME = process.env.DB_NAME || 'api_core';
var DB_PASS = process.env.DB_PASSWORD || 'root';

CONNECTION_ERROR = 'Database connection failed. Is database server running and listening on port ' + DB_PORT + '?';

function serialize(obj) {
  var str = [];
  for (var prop in obj) {
    if (obj.hasOwnProperty(prop)) {
      str.push(encodeURIComponent(prop) + '=' + encodeURIComponent(obj[prop]));
    }
  }
  return str.join('&');
}

var db = mysql.createConnection({
  host : DB_HOST,
  port : DB_PORT,
  user : DB_USER,
  password : DB_PASS,
  database : DB_NAME
});

db.connect();

function disconnect() {
  if (db) db.end();
}

function query(sql, values) {
  return function() {
    return new Promise(function(resolve, reject) {
      var callback = function(error, results, fields) {
        if (error) {
          reject(error);
        } else {
          resolve(results);
        }
      };
      if ('undefined' === typeof(values)) {
        db.query(sql, callback);
      } else {
        db.query(sql, values, callback);
      }
    });
  }
}

function truncate() {
  return Promise.resolve()
  .then(query('SET FOREIGN_KEY_CHECKS=0;'))
  .then(query('TRUNCATE TABLE uliza_participant_registration_status_events;'))
  .then(query('TRUNCATE TABLE uliza_participants;'))
  .then(query('TRUNCATE TABLE uliza_registration_calls;'))
  .then(query('TRUNCATE TABLE eav_attribute;'))
  .then(query('TRUNCATE TABLE eav_enumgroup;'))
  .then(query('TRUNCATE TABLE eav_enumgroup_enums;'))
  .then(query('TRUNCATE TABLE eav_enumvalue;'))
  .then(query('TRUNCATE TABLE eav_value;'))
  .then(query('SET FOREIGN_KEY_CHECKS=1;'));
}

function loadFixtures() {
  var inserts = glob.sync('./fixtures/*.json').map(function(file) {
    var fixtures = require(path.resolve(file));
    return fixtures.data.map(function(values) {
      var sql = 'INSERT INTO ' + fixtures.table + ' SET ?';
      return query(sql, values)();
    })
  });
  return Promise.all(Array.prototype.concat.apply([], inserts));
}

function normalizePhoneNumber(s) {
  if (s.length && '+' !== s[0]) {
    return '+' + s;
  }
}

module.exports = {
  disconnect: disconnect,
  serialize: serialize,
  query: query,
  truncate: truncate,
  loadFixtures: loadFixtures,
  normalizePhoneNumber: normalizePhoneNumber,
  CONNECTION_ERROR: CONNECTION_ERROR
};
