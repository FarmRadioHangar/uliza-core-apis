var util  = require('util');
var tests = require('./utils');

var response_001 = { 
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

var call_status_update_001 = {
  subscriber_id: "1",
  subscriber_phone: "255678647268",
  delivery_status: "6"
};

var call_status_update_002 = {
  subscriber_id: 1,
  subscriber_phone: "+255678647268",
  delivery_status: 6
};

//var call_status_update_003 = {
//  subscriber_id: "1",
//  subscriber_phone: "255678647268",
//  delivery_status: "6"
//};
//
//var call_status_update_004 = {
//  subscriber_id: "1",
//  subscriber_phone: "255678647268",
//  delivery_status: "5"
//};
//
//var call_status_update_005 = {
//  subscriber_id: "1",
//  subscriber_phone: "255999999999",
//  delivery_status: "5"
//};

function runner(data, next) {
  return tests.request()
  .post('/call_status_update')
  .set('Accept', 'application/json')
  .send(data)
  .then(next);
}

describe('Call status update request', function() {

  this.timeout(6000);

  beforeEach(function() { 
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp); 
  });

  afterEach(tests.schemaDown);

  it('should return a 200 OK JSON-formatted response', function() {
    return runner(call_status_update_001, function(response) {
      response.should.have.header('Content-Type', /json/);
      response.status.should.equal(200);
    });
  });

  it('should accept a request object with non-string values', function() {
    return runner(call_status_update_001, function(response) {
      response.should.have.header('Content-Type', /json/);
      response.status.should.equal(200);
    });
  });

});

describe('Bad request format', function() {

  it('should return a status code 500', function() {
    return runner({}, function(response) {
      response.should.have.header('Content-Type', /json/);
      response.status.should.equal(500);
    });
  });

});


describe('Call status update request', function() {

  this.timeout(6000);

  beforeEach(function() { 
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp)
    .then(function() {
      return tests.request()
      .post('/responses')
      .set('Accept', 'application/json')
      .send(response_001);
    });
  });

  afterEach(tests.schemaDown);

  it('should change a participant\'s registration_status to REGISTERED', function() {
    return runner(call_status_update_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.participants;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        var participant = results.rows[0];
        participant.should.have.property('registration_status').equal('REGISTERED');
      });
    });
  });

  it('should have a response with a registration_status property set to REGISTERED', function() {
    return runner(call_status_update_001, function(response) {
      response.body.should.have.property('data'); 
      var participant = response.body.data;
      participant.should.have.property('registration_status').equal('REGISTERED');
    });
  });

});

describe('Call status update request (+ prefix)', function() {

  this.timeout(6000);

  beforeEach(function() { 
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp)
    .then(function() {
      return tests.request()
      .post('/responses')
      .set('Accept', 'application/json')
      .send(response_001);
    });
  });

  afterEach(tests.schemaDown);

  it('should change a participant\'s registration_status to REGISTERED', function() {
    return runner(call_status_update_002, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.participants;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        var participant = results.rows[0];
        participant.should.have.property('registration_status').equal('REGISTERED');
      });
    });
  });

  it('should have a response with a registration_status property set to REGISTERED', function() {
    return runner(call_status_update_002, function(response) {
      response.body.should.have.property('data'); 
      var participant = response.body.data;
      participant.should.have.property('registration_status').equal('REGISTERED');
    });
  });

});
