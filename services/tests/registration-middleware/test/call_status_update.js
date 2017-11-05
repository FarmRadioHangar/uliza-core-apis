require('dotenv').config();

var mocha   = require('mocha');
var mysql   = require('mysql');
var request = require('supertest');
var tests   = require('./integration');
var util    = require('util');
var qs      = require('qs')
var up      = require('../utils/up');
var down    = require('../utils/down');

function makeRunner(data) {
  return function() {
    return request(process.env.REG_SERVICE_URL)
    .post('/call_status_updates')
    .set('Content-Type', 'application/x-www-form-urlencoded')
    .set('Accept', 'application/json')
    .send(tests.serialize(data));
  }
}

describe('/call_status_updates', function() {

  var self = this;

  var query = function(sql) {
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

  var flushDb = function() {
    return Promise.resolve()
    .then(self._db.query('DELETE FROM uliza_participant_registration_status_log;'))
    .then(self._db.query('DELETE FROM uliza_voto_webhook_log;'))
    .then(self._db.query('DELETE FROM uliza_participants;'))
    .then(self._db.query('DELETE FROM uliza_registration_calls;'))
    .catch(console.error);
  };

  this.timeout(4000000);

  before(up);

  after(down);

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
      return self._db.connect();
    })
    .then(flushDb)
//    .then(function() {
//      if ('function' === typeof(self._hook)) {
//        return self._hook();
//      }
//    })
    .catch(console.error);
  });

  afterEach(function() { 
    return Promise.resolve()
    .then(function() {
      self._db.end();
    });
  });

  describe('Call status update with delivery_status = 6', function() {

    var runner = makeRunner({
      subscriber_phone: "255678647268",
      subscriber_id: "3",
      delivery_status: "6"
    });

    it('should return a 200 OK JSON-formatted response', function() {
      return runner()
      .then(function(response) { 
        response.should.have.header('Content-Type', /json/);
        response.status.should.equal(200);
      });
    });

    it('should create a uliza_voto_webhook_log entry in the database', function() {
      return runner()
      .then(query('SELECT * FROM uliza_voto_webhook_log;'))
      .then(function(results) {
        results.length.should.equal(1);
        var row = qs.parse(results[0].data);
        row.should.deep.equal({
          subscriber_phone: "255678647268",
          subscriber_id: "3",
          delivery_status: "6"
        });
      });
    });

  });

  describe('Call status update with delivery_status = 5', function() {

    var runner = makeRunner({
      subscriber_phone: "255678647268",
      subscriber_id: "3",
      delivery_status: "5"
    });

    it('should return a 200 OK JSON-formatted response', function() {
      return runner()
      .then(function(response) { 
        response.should.have.header('Content-Type', /json/);
        response.status.should.equal(200);
      });
    });

    it('should create a uliza_voto_webhook_log entry in the database', function() {
      return runner()
      .then(query('SELECT * FROM uliza_voto_webhook_log;'))
      .then(function(results) {
        results.length.should.equal(1);
        var row = qs.parse(results[0].data);
        row.should.deep.equal({
          subscriber_phone: "255678647268",
          subscriber_id: "3",
          delivery_status: "5"
        });
      });
    });

  });

  describe('Call status update with delivery_status = x', function() {

    var runner = makeRunner({
      subscriber_phone: "255678647268",
      subscriber_id: "3",
      delivery_status: "x"
    });

    it('should return a status code 400', function() {
      return runner()
      .then(function(response) { 
        response.should.have.header('Content-Type', /json/);
        response.status.should.equal(400);
      });
    });

    it('should create a uliza_voto_webhook_log entry in the database', function() {
      return runner()
      .then(query('SELECT * FROM uliza_voto_webhook_log;'))
      .then(function(results) {
        results.length.should.equal(1);
        var row = qs.parse(results[0].data);
        row.should.deep.equal({
          subscriber_phone: "255678647268",
          subscriber_id: "3",
          delivery_status: "x"
        });
      });
    });

  });

  describe('Bad request format', function() {

    var runner = makeRunner({});

    it('should return a status code 400', function() {
      return runner()
      .then(function(response) { 
        response.should.have.header('Content-Type', /json/);
        response.status.should.equal(400);
      });
    });

  });

});

//var util  = require('util');
//var tests = require('./utils');

//var response_001 = { 
//  question_id: "127375", 
//  survey_id: "89324", 
//  voto_id: "44", 
//  response_type: "1", 
//  content_type: "1", 
//  poll_id: "213", 
//  delivery_log_id: "832", 
//  choice_id: "1", 
//  subscriber_id: "232", 
//  subscriber_phone: "+255678647268", 
//  question_title: "Who was the first man to set foot on the moon?", 
//  choice_name: "Neil Armstrong", 
//  date_received: "2017-07-24T18:13:51Z"
//};
//
//var call_status_update_001 = {
//  subscriber_id: "1",
//  subscriber_phone: "255678647268",
//  delivery_status: "6"
//};
//
//var call_status_update_002 = {
//  subscriber_id: 1,
//  subscriber_phone: "+255678647268",
//  delivery_status: 6
//};
//
////var call_status_update_003 = {
////  subscriber_id: "1",
////  subscriber_phone: "255678647268",
////  delivery_status: "6"
////};
////
////var call_status_update_004 = {
////  subscriber_id: "1",
////  subscriber_phone: "255678647268",
////  delivery_status: "5"
////};
////
////var call_status_update_005 = {
////  subscriber_id: "1",
////  subscriber_phone: "255999999999",
////  delivery_status: "5"
////};
//
//function runner(data, next) {
//  return tests.request()
//  .post('/call_status_update')
//  .set('Accept', 'application/json')
//  .send(data)
//  .then(next);
//}
//
//describe('Call status update request', function() {
//
//  this.timeout(6000);
//
//  beforeEach(function() { 
//    return tests.unlock()
//    .then(tests.schemaDown)
//    .then(tests.schemaUp); 
//  });
//
//  afterEach(tests.schemaDown);
//
//  it('should return a 200 OK JSON-formatted response', function() {
//    return runner(call_status_update_001, function(response) {
//      response.should.have.header('Content-Type', /json/);
//      response.status.should.equal(200);
//    });
//  });
//
//  it('should accept a request object with non-string values', function() {
//    return runner(call_status_update_001, function(response) {
//      response.should.have.header('Content-Type', /json/);
//      response.status.should.equal(200);
//    });
//  });
//
//});
//
//describe('Bad request format', function() {
//
//  it('should return a status code 500', function() {
//    return runner({}, function(response) {
//      response.should.have.header('Content-Type', /json/);
//      response.status.should.equal(500);
//    });
//  });
//
//});
//
//
//describe('Call status update request', function() {
//
//  this.timeout(6000);
//
//  beforeEach(function() { 
//    return tests.unlock()
//    .then(tests.schemaDown)
//    .then(tests.schemaUp)
//    .then(function() {
//      return tests.request()
//      .post('/responses')
//      .set('Accept', 'application/json')
//      .send(response_001);
//    });
//  });
//
//  afterEach(tests.schemaDown);
//
//  it('should change a participant\'s registration_status to REGISTERED', function() {
//    return runner(call_status_update_001, function(response) {
//      return tests.db.query('SELECT * FROM farmradio_api.participants;')
//      .then(function(results) {
//        results.rowCount.should.equal(1);
//        var participant = results.rows[0];
//        participant.should.have.property('registration_status').equal('REGISTERED');
//      });
//    });
//  });
//
//  it('should have a response with a registration_status property set to REGISTERED', function() {
//    return runner(call_status_update_001, function(response) {
//      response.body.should.have.property('data'); 
//      var participant = response.body.data;
//      participant.should.have.property('registration_status').equal('REGISTERED');
//    });
//  });
//
//});
//
//describe('Call status update request (+ prefix)', function() {
//
//  this.timeout(6000);
//
//  beforeEach(function() { 
//    return tests.unlock()
//    .then(tests.schemaDown)
//    .then(tests.schemaUp)
//    .then(function() {
//      return tests.request()
//      .post('/responses')
//      .set('Accept', 'application/json')
//      .send(response_001);
//    });
//  });
//
//  afterEach(tests.schemaDown);
//
//  it('should change a participant\'s registration_status to REGISTERED', function() {
//    return runner(call_status_update_002, function(response) {
//      return tests.db.query('SELECT * FROM farmradio_api.participants;')
//      .then(function(results) {
//        results.rowCount.should.equal(1);
//        var participant = results.rows[0];
//        participant.should.have.property('registration_status').equal('REGISTERED');
//      });
//    });
//  });
//
//  it('should have a response with a registration_status property set to REGISTERED', function() {
//    return runner(call_status_update_002, function(response) {
//      response.body.should.have.property('data'); 
//      var participant = response.body.data;
//      participant.should.have.property('registration_status').equal('REGISTERED');
//    });
//  });
//
//});
