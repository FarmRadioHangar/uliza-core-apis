var setup   = require('./integration');
var request = require('supertest');
var mocha   = require('mocha');

function stripPrefix(s) {
  if (s.length && '+' === s[0]) {
    return s.substring(1);
  }
  return s;
}

describe('Response from new participant', function() {

  setup(this);

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

});
