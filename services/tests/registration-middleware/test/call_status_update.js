require('dotenv').config();

var assert  = require('assert');
var chai    = require('chai');
var mocha   = require('mocha');
var mysql   = require('mysql');
var qs      = require('qs');
var request = require('supertest');
var util    = require('util');
var up      = require('../utils/containers-up');
var down    = require('../utils/containers-down');

var REG_SERVICE_URL = process.env.REG_SERVICE_URL || 'http://0.0.0.0:3034';
var DB_HOST         = process.env.DB_HOST || '0.0.0.0';
var DB_PORT         = process.env.DB_PORT || 3316;
var ULIZA_API_URL   = 'http://0.0.0.0:8000/api/v1';
var VOTO_API_URL    = 'http://0.0.0.0:8089/api/v1';
var VOTO_API_KEY    = process.env.VOTO_API_KEY || 'xxxxxxxxxxxxxxxxxxxxxxxxx';

chai.should();
chai.use(require('chai-things'));
chai.use(require('chai-also'));
chai.use(require('chai-http'));

function serialize(obj) {
  var str = [];
  for (var p in obj) {
    if (obj.hasOwnProperty(p)) {
      str.push(encodeURIComponent(p) + '=' + encodeURIComponent(obj[p]));
    }
  }
  return str.join('&');
}

function makeRunner(data) {
  return function() {
    return request(REG_SERVICE_URL)
    .post('/call_status_updates')
    .set('Content-Type', 'application/x-www-form-urlencoded')
    .set('Accept', 'application/json')
    .send(serialize(data));
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
    .then(query('SET FOREIGN_KEY_CHECKS=0;'))
    .then(query('TRUNCATE TABLE uliza_participant_registration_status_log;'))
    .then(query('TRUNCATE TABLE uliza_voto_webhook_log;'))
    .then(query('TRUNCATE TABLE uliza_participants;'))
    .then(query('TRUNCATE TABLE uliza_registration_calls;'))
    .then(query('TRUNCATE TABLE eav_attribute;'))
    .then(query('TRUNCATE TABLE eav_enumgroup;'))
    .then(query('TRUNCATE TABLE eav_enumgroup_enums;'))
    .then(query('TRUNCATE TABLE eav_enumvalue;'))
    .then(query('TRUNCATE TABLE eav_value;'))
    .then(query('SET FOREIGN_KEY_CHECKS=1;'))
    .catch(console.error);
  };

  this.timeout(4000000);

  //before(function() {
  //  return up();
  //});

  //after(function() {
  //  return down();
  //});

  beforeEach(function() { 
    return Promise.resolve()
    .then(function() {
      self._db = mysql.createConnection({
        host     : DB_HOST,
        port     : DB_PORT,
        user     : 'root',
        password : 'root',
        database : 'api_core'
      });
      return self._db.connect();
    })
    .then(flushDb)
    .then(function() {
      return request(REG_SERVICE_URL)
      .post('/responses')
      .set('Content-Type', 'application/x-www-form-urlencoded')
      .set('Accept', 'application/json')
      .send(serialize({
        question_id: "127375", 
        survey_id: "89324", 
        voto_id: "44", 
        response_type: "1", 
        content_type: "1", 
        poll_id: "213", 
        delivery_log_id: "832", 
        choice_id: "1", 
        subscriber_id: "232", 
        subscriber_phone: "+255678647268", 
        question_title: "Who was the first man to set foot on the moon?", 
        choice_name: "Neil Armstrong", 
        date_received: "2017-07-24T18:13:51Z"
      }))
    })
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
      .then(query('SELECT * FROM uliza_voto_webhook_log WHERE endpoint = "call_status_updates";'))
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

    it('should change a participant\'s registration_status to REGISTERED', function() {
      return runner()
      .then(query('SELECT * FROM uliza_participants;'))
      .then(function(results) {
        results.length.should.equal(1);
        var participant = qs.parse(results[0]);
        participant.should.have.property('registration_status').equal('REGISTERED');
      });
    });

    it('should have a response with a registration_status property set to REGISTERED', function() {
      return runner()
      .then(function(response) { 
        response.body.should.have.property('data'); 
        var participant = response.body.data;
        participant.should.have.property('registration_status').equal('REGISTERED');
      });
    });

    it('should have assigned registration attributes to the participant', function() {
      return runner()
      .then(request(ULIZA_API_URL)
        .get('/participants?phone_number=255678647268')
        .set('Accept', 'application/json')
        .send())
      .then(function(response) { 
        response.body.should.have.property('data'); 
        var participant = response.body.data;
        participant.should.have.property('attributes');
        participant.attributes.should.be.an('object');
        participant.attributes.should.have.property('location').equal('Arusha');
        participant.attributes.should.have.property('gender').equal('Female');
        participant.attributes.should.have.property('occupation').equal('Farmer');
        participant.attributes.should.not.have.property('shoe_size');
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
      .then(query('SELECT * FROM uliza_voto_webhook_log WHERE endpoint = "call_status_updates";'))
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

    it('should NOT change the participant\'s registration_status to REGISTERED', function() {
      return runner()
      .then(query('SELECT * FROM uliza_participants;'))
      .then(function(results) {
        results.length.should.equal(1);
        var participant = qs.parse(results[0]);
        participant.should.have.property('registration_status').equal('NOT_REGISTERED');
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
      .then(query('SELECT * FROM uliza_voto_webhook_log WHERE endpoint = "call_status_updates";'))
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

    it('should NOT change the participant\'s registration_status to REGISTERED', function() {
      return runner()
      .then(query('SELECT * FROM uliza_participants;'))
      .then(function(results) {
        results.length.should.equal(1);
        var participant = qs.parse(results[0]);
        participant.should.have.property('registration_status').equal('NOT_REGISTERED');
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
