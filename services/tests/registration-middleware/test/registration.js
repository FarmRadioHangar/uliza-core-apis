var mocha   = require('mocha');
var mysql   = require('mysql');
var request = require('supertest');
var tests   = require('./integration');

function stripPrefix(s) {
  if (s.length && '+' === s[0]) {
    return s.substring(1);
  }
  return s;
}

function query(sql) {
  return function() {
    return new Promise(function(resolve, reject) {
      var db = mysql.createConnection({
        host     : '0.0.0.0',
        port     : 3316,
        user     : 'root',
        password : 'root',
        database : 'api_core'
      });
      db.connect();
      db.query(sql, function(error, results, fields) {
        if (error) {
          reject(error);
        } else {
          resolve(results);
        }
      });
      db.end();
    });
  }
}

describe('Response from new participant', function() {

  tests.init(this);

  var self = this;

  var data = {
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
  };

  var runner = function() {
    return request(process.env.REG_SERVICE_URL)
    .post('/responses')
    .set('Accept', 'application/json')
    .send(data);
  }

  it('should return a 200 OK JSON-formatted response', function() {
    return runner()
    .then(function(response) { 
      response.should.have.header('Content-Type', /json/);
      response.status.should.equal(200);
    });
  });

  it('should return a REGISTRATION_CALL_SCHEDULED response', function() {
    return runner()
    .then(function(response) { 
      response.body.should.have.property('action').equal('REGISTRATION_CALL_SCHEDULED');
    });
  });

  it('should return a scheduled call with the participant\'s phone number', function() {
    return runner()
    .then(function(response) { 
      response.body.should.have.property('registration_call');
      response.body.registration_call.should.have.property('phone_number').equal(stripPrefix(data.subscriber_phone));
    });
  });

  it('should create a voto_response_data entry in the database', function() {
    return runner()
    .then(query('SELECT * FROM uliza_voto_response_data;'))
    .then(function(results) {
      results.length.should.equal(1);
      var row = JSON.parse(results[0].data);
      row.should.deep.equal(data);
    });
  });

  it('should create a participant_registration_status_log entry in the database', function() {
    return runner()
    .then(query('SELECT * FROM uliza_participant_registration_status_log;'))
    .then(function(results) {
      results.length.should.equal(1);
      results[0].should.have.a.property('event_type').equal('REGISTRATION_CALL_SCHEDULED');
    });
  });

});
