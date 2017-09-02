var mocha   = require('mocha');
var mysql   = require('mysql');
var request = require('supertest');
var tests   = require('./integration');
var util    = require('util');

function stripPrefix(s) {
  if (s.length && '+' === s[0]) {
    return s.substring(1);
  }
  return s;
}

function toDateString(d) {
  return d.toISOString().substring(0, 19).replace('T', ' ');
}

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

describe('Response from new participant', function() {

  var self = this;

  tests.init(this);

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
    .then(self.query('SELECT * FROM uliza_voto_response_data;'))
    .then(function(results) {
      results.length.should.equal(1);
      var row = JSON.parse(results[0].data);
      row.should.deep.equal(data);
    });
  });

  it('should create a participant_registration_status_log entry in the database', function() {
    return runner()
    .then(self.query('SELECT * FROM uliza_participant_registration_status_log;'))
    .then(function(results) {
      results.length.should.equal(1);
      results[0].should.have.a.property('event_type').equal('REGISTRATION_CALL_SCHEDULED');
    });
  });

  it('should set the participant\'s registration_call_id', function() {
    var registrationCall = null;
    return runner()
    .then(self.query('SELECT * FROM uliza_registration_calls;'))
    .then(function(results) {
      results.length.should.equal(1);
      registrationCall = results[0];
      registrationCall.should.be.an('object');
    })
    .then(self.query(util.format(
      'SELECT * FROM uliza_participants WHERE phone_number=\'%s\';', 
      stripPrefix(data.subscriber_phone)
    )))
    .then(function(results) {
      results.length.should.equal(1);
      var participant = results[0];
      participant.registration_call_id.should.be.a('number');
      participant.registration_call_id.should.equal(registrationCall.id);
    })
    .then(self.query('SELECT * FROM uliza_participant_registration_status_log;'))
    .then(function(results) {
      var logEntry = results[0];
      logEntry.registration_call_id.should.be.a('number');
      logEntry.registration_call_id.should.equal(registrationCall.id);
    })
  });

});

describe('Response from an already registered participant', function() {

  var self = this;

  tests.init(this, function() {
    return Promise.resolve()
    .then(self.query('INSERT INTO uliza_registration_calls (phone_number, scheduled_time, created_at) VALUES (\'255678647268\', \'2017-07-24 18:13:51\', \'2017-07-24 18:03:51\');'))
    .then(self.query('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'REGISTERED\', LAST_INSERT_ID(), \'2017-07-24 18:04:51\');'));
  });

  it('should return ALREADY_REGISTERED', function() {
    return runner()
    .then(function(response) {
      response.body.should.have.property('message').equal('ALREADY_REGISTERED');
    });
  });

  it('should not create any registration call', function() {
    return runner()
    .then(function(response) {
      return Promise.resolve()
      .then(self.query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) { 
        results.length.should.equal(1); 
      })
      .then(self.query('SELECT * FROM uliza_participants;'))
      .then(function(results) { 
        results.length.should.equal(1); 
      });
    });
  });

});

describe('Response from a participant who has declided to be registered', function() {

  var self = this;

  tests.init(this, function() {
    return Promise.resolve()
    .then(self.query('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'DECLINED\', NULL, \'2017-07-24 18:03:51\');'));
  });

  it('should return REGISTRATION_DECLINED', function() {
    return runner()
    .then(function(response) {
      response.body.should.have.property('message').equal('REGISTRATION_DECLINED');
    });
  });

  it('should not create any registration call', function() {
    return runner()
    .then(function(response) {
      return Promise.resolve()
      .then(self.query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) { 
        results.length.should.equal(0); 
      })
      .then(self.query('SELECT * FROM uliza_participants;'))
      .then(function(results) { 
        results.length.should.equal(1); 
      });
    });
  });

});

describe('Response from a participant for whom a registration call is already due', function() {

  var self = this;

  tests.init(this, function() {
    var d = new Date();
    var createdAt = toDateString(d);
    d.setMinutes(d.getMinutes() + 20);
    var scheduledAt = toDateString(d);
    return Promise.resolve()
    .then(self.query(util.format('INSERT INTO uliza_registration_calls (phone_number, created_at, scheduled_time) VALUES (\'255678647268\', \'%s\', \'%s\');', createdAt, scheduledAt)))
    .then(self.query(util.format('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'NOT_REGISTERED\', LAST_INSERT_ID(), \'%s\');', createdAt)));
  });

  it('should return PRIOR_CALL_SCHEDULED', function() {
    return runner()
    .then(function(response) {
      response.body.should.have.property('message').equal('PRIOR_CALL_SCHEDULED');
    });
  });

  it('should not create any registration call', function() {
    return runner()
    .then(function(response) {
      return Promise.resolve()
      .then(self.query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) {
        results.length.should.equal(1); 
      })
      .then(self.query('SELECT * FROM uliza_participants;'))
      .then(function(results) {
        results.length.should.equal(1); 
      });
    });
  });

});

describe('Response from a participant for whom a registration call happened recently', function() {

  var self = this;

  tests.init(this, function() {
    var d = new Date();
    var createdAt = toDateString(d);
    return Promise.resolve()
    .then(self.query(util.format('INSERT INTO uliza_registration_calls (phone_number, created_at, scheduled_time) VALUES (\'255678647268\', \'%s\', \'%s\');', createdAt, createdAt)))
    .then(self.query(util.format('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'NOT_REGISTERED\', LAST_INSERT_ID(), \'%s\');', createdAt)));
  });

  it('should return TOO_SOON', function() {
    return runner()
    .then(function(response) {
      response.body.should.have.property('message').equal('TOO_SOON');
    });
  });

  it('should not create any registration call', function() {
    return runner()
    .then(function(response) {
      return Promise.resolve()
      .then(self.query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) {
        results.length.should.equal(1); 
      })
      .then(self.query('SELECT * FROM uliza_participants;'))
      .then(function(results) {
        results.length.should.equal(1); 
      })
    });
  });

});

describe('Response from a participant for whom the most recent call took place 1 day ago', function() {

  var self = this;

  tests.init(this, function() {
    var d = new Date();
    var createdAt = toDateString(d);
    d.setMinutes(d.getMinutes() - 60*24*1);
    var scheduledAt = toDateString(d);
    return Promise.resolve()
    .then(self.query(util.format('INSERT INTO uliza_registration_calls (phone_number, created_at, scheduled_time) VALUES (\'255678647268\', \'%s\', \'%s\');', createdAt, scheduledAt)))
    .then(self.query(util.format('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'NOT_REGISTERED\', LAST_INSERT_ID(), \'%s\');', createdAt)));
  });

  it('should return TOO_SOON', function() {
    return runner()
    .then(function(response) {
      response.body.should.have.property('message').equal('TOO_SOON');
    });
  });

  it('should not create any registration call', function() {
    return runner()
    .then(function(response) {
      return Promise.resolve()
      .then(self.query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) {
        results.length.should.equal(1); 
      })
      .then(self.query('SELECT * FROM uliza_participants;'))
      .then(function(results) {
        results.length.should.equal(1); 
      })
    });
  });

});

describe('Response from a participant for whom the most recent call took place more than 2 days ago', function() {

  var self = this;

  tests.init(this, function() {
    var d = new Date();
    var createdAt = toDateString(d);
    d.setMinutes(d.getMinutes() - 60*24*2);
    var scheduledAt = toDateString(d);
    return Promise.resolve()
    .then(self.query(util.format('INSERT INTO uliza_registration_calls (phone_number, created_at, scheduled_time) VALUES (\'255678647268\', \'%s\', \'%s\');', createdAt, scheduledAt)))
    .then(self.query(util.format('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'NOT_REGISTERED\', LAST_INSERT_ID(), \'%s\');', createdAt)));
  });

  it('should return a scheduled call with the participant\'s phone number', function() {
    return runner()
    .then(function(response) {
      response.body.registration_call.should.have.property('phone_number').equal(stripPrefix(data.subscriber_phone));
    });
  });

  it('should create a participant_registration_status_log entry in the database', function() {
    return runner()
    .then(function(response) {
      return Promise.resolve()
      .then(self.query('SELECT * FROM uliza_participant_registration_status_log;'))
      .then(function(results) {
        results.length.should.equal(1);
        results[0].should.have.a.property('event_type').equal('REGISTRATION_CALL_SCHEDULED');
      });
    });
  });

  it('should create a new registration call in the database', function() {
    return runner()
    .then(function(response) {
      return Promise.resolve()
      .then(self.query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) {
        results.length.should.equal(2);
      });
    });
  });

  it('should update the participant\'s registration_call_id', function() {
    var registrationCall = null;
    return runner()
    .then(function(response) {
      return Promise.resolve()
      .then(self.query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) {
        registrationCall = results[1];
        registrationCall.should.be.an('object');
        return ;
      })
      .then(self.query(util.format(
        'SELECT * FROM uliza_participants WHERE phone_number=\'%s\';', 
        stripPrefix(data.subscriber_phone)
      )))
      .then(function(results) {
        results.length.should.equal(1);
        var participant = results[0];
        participant.registration_call_id.should.be.a('number');
        participant.registration_call_id.should.equal(registrationCall.id);
      })
      .then(self.query('SELECT * FROM uliza_participant_registration_status_log;'))
      .then(function(results) {
        var logEntry = results[0];
        logEntry.registration_call_id.should.be.a('number');
        logEntry.registration_call_id.should.equal(registrationCall.id);
      });
    });
  });

});

describe('Bad request format', function() {

  var self = this;

  tests.init(this);

  it('should return a status code 500', function() {
    return request(process.env.REG_SERVICE_URL)
    .post('/responses')
    .set('Accept', 'application/json')
    .send({})
    .then(function(response) {
      response.should.have.header('Content-Type', /json/);
      response.status.should.equal(500);
    });
  });

});

describe('Phone number with + prefix', function() {

  var self = this;

  tests.init(this);

  it('should be stripped off before inserted into database', function() {
    return runner()
    .then(function(response) {
      return Promise.resolve()
      .then(self.query('SELECT * FROM uliza_participants;'))
      .then(function(results) {
        var participant = results[0];
        participant.phone_number.should.equal(stripPrefix(data.subscriber_phone));
      });
    });
  });

  it('should appear without + in response', function() {
    return runner()
    .then(function(response) {
      response.body.registration_call.should.have.property('phone_number')
      .equal(stripPrefix(data.subscriber_phone));
    });
  });

});
