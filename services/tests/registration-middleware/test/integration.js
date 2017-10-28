require('dotenv').config();

var Docker  = require('simple-dockerode');
var assert  = require('assert');
var chai    = require('chai');
var mocha   = require('mocha');
var mysql   = require('mysql');
var path    = require('path');
var up      = require('../utils/up');
var down    = require('../utils/down');

chai.should();
chai.use(require('chai-things'));
chai.use(require('chai-also'));
chai.use(require('chai-http'));

var docker = new Docker();

var init = function(self, hook) {

  self.timeout(4000000);

  self._containers = {};
  self._hook = hook;

  var flushDb = function() {
    return Promise.resolve()
    .then(self.query('DELETE FROM uliza_participant_registration_status_log;'))
    .then(self.query('DELETE FROM uliza_voto_webhook_log;'))
    .then(self.query('DELETE FROM uliza_participants;'))
    .then(self.query('DELETE FROM uliza_registration_calls;'))
    .catch(console.error);
  };

  var ping = function(cmd, msg) {
    return new Promise(function(resolve, reject) {
      var options = {stdout: true, stderr: true};
      var retry = function() {
        self._containers.api.exec(cmd, options, 
        function(err, results) {
          if (err) {
            return reject(err);
          }
          process.stdout.write('.');
          if (results.inspect.ExitCode) {
            retry();
          } else {
            console.log('\n' + msg);
            resolve();
          }
        });
      }
      retry();
    });
  };

  self.query = function(sql) {
    return function() {
      return new Promise(function(resolve, reject) {
        if (self._db) {
          self._db.query(sql, function(error, results, fields) {
            if (error) {
              reject(error);
            } else {
              resolve(results);
            }
          });
        }
      });
    }
  };

  before(function() { 
    return up();
  });

  after(function() { 
    return down();
  });

  beforeEach(function() { 
    return Promise.resolve()
    .then(function() {
      self._db = mysql.createConnection({
        host     : '0.0.0.0',
        port     : 3316,
        user     : 'root',
        password : 'root',
        database : 'api_core'
      });
      self._db.connect();
    })
    .then(flushDb)
    .then(function() {
      if ('function' === typeof(self._hook)) {
        return self._hook();
      }
    })
    .catch(console.error);
  });

  afterEach(function() { 
    return Promise.resolve().then(function() {
      self._db.end();
    });
  });

}

module.exports = {
  init: init
};
