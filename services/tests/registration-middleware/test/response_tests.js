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

function runner(data, next) {
  return tests.request()
  .post('/responses')
  .set('Accept', 'application/json')
  .send(data)
  .then(next);
}

function stripPrefix(s) {
  if (s.length && '+' === s[0]) {
    return s.substring(1);
  }
  return s;
}

describe('Response from new participant', function() {

  this.timeout(6000);

  beforeEach(function() { 
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp); 
  });

  afterEach(tests.schemaDown);

  it('should return a 200 OK JSON-formatted response', function() {
    return runner(response_001, function(response) {
      response.should.have.header('Content-Type', /json/);
      response.status.should.equal(200);
    });
  });

  it('should return a scheduled call with the participant\'s phone number', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('phone_number').equal(stripPrefix(response_001.subscriber_phone));
    });
  });

  it('should create a voto_response_data entry in the database', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.voto_response_data;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        results.rows[0].data.should.deep.equal(response_001);
      });
    });
  });

  it('should create a participant_registration_status_log entry in the database', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.participant_registration_status_log;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        results.rows[0].should.have.a.property('event_type').equal('REGISTRATION_CALL_SCHEDULED');
      });
    });
  });

  it('should set the participant\'s registration_call_id', function() {
    return runner(response_001, function(response) {
      var registrationCall = null;
      return tests.db.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        registrationCall = results.rows[0];
        registrationCall.should.be.an('object');
        return tests.db.query(util.format(
          "SELECT * FROM farmradio_api.participants WHERE phone_number='%s';", 
          stripPrefix(response_001.subscriber_phone)
        ));
      })
      .then(function(results) {
        results.rowCount.should.equal(1);
        var participant = results.rows[0];
        participant.registration_call_id.should.be.a('number');
        participant.registration_call_id.should.equal(registrationCall.id);
        return tests.db.query('SELECT * FROM farmradio_api.participant_registration_status_log;')
      })
      .then(function(results) {
        var logEntry = results.rows[0];
        logEntry.registration_call_id.should.be.a('number');
        logEntry.registration_call_id.should.equal(registrationCall.id);
      });
    });
  });

});

describe('Response from an already registered participant', function() {

  this.timeout(6000);

  beforeEach(function() {
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp)
    .then(function() {
      return tests.db.query(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('255678647268', '2017-07-24T18:13:51Z') RETURNING id;");
    })
    .then(function(results) {
      return tests.db.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('255678647268', 'REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(tests.schemaDown);

  it('should return PARTICIPANT_ALREADY_REGISTERED', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('PARTICIPANT_ALREADY_REGISTERED');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      })
      .then(function() { 
        return tests.db.query('SELECT * FROM farmradio_api.participants;');
      })
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      });
    });
  });

});

describe('Response from a participant who has declided to be registered', function() {

  this.timeout(6000);

  beforeEach(function() {
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp)
    .then(function() {
      return tests.db.query(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status)\
         \ VALUES ('255678647268', 'DECLINED');"
      );
    });
  });

  afterEach(tests.schemaDown);

  it('should return REGISTRATION_DECLINED', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('REGISTRATION_DECLINED');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(0); 
      })
      .then(function() { 
        return tests.db.query('SELECT * FROM farmradio_api.participants;');
      })
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      });
    });
  });

});

describe('Response from a participant for whom a registration call is already due', function() {

  this.timeout(6000);

  beforeEach(function() {
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp)
    .then(function() {
      var d = new Date();
      d.setMinutes(d.getMinutes() + 20);
      return tests.db.query(util.format(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('255678647268', '%s') RETURNING id;", d.toISOString()));
    })
    .then(function(results) {
      return tests.db.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('255678647268', 'NOT_REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(tests.schemaDown);

  it('should return PRIOR_CALL_SCHEDULED', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('PRIOR_CALL_SCHEDULED');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      })
      .then(function() { 
        return tests.db.query('SELECT * FROM farmradio_api.participants;');
      })
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      });
    });
  });

});

describe('Response from a participant for whom a registration call happened recently', function() {

  this.timeout(6000);

  beforeEach(function() {
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp)
    .then(function() {
      var d = new Date();
      return tests.db.query(util.format(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('255678647268', '%s') RETURNING id;", d.toISOString()));
    })
    .then(function(results) {
      return tests.db.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('255678647268', 'NOT_REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(tests.schemaDown);

  it('should return RECENT_CALL_MADE', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('RECENT_CALL_MADE');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      })
      .then(function() { 
        return tests.db.query('SELECT * FROM farmradio_api.participants;');
      })
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      });
    });
  });

});

describe('Response from a participant for whom the most recent call took place 1 day ago', function() {

  this.timeout(6000);

  beforeEach(function() {
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp)
    .then(function() {
      var d = new Date();
      d.setMinutes(d.getMinutes() - 60*24*1);
      return tests.db.query(util.format(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('255678647268', '%s') RETURNING id;", d.toISOString()));
    })
    .then(function(results) {
      return tests.db.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('255678647268', 'NOT_REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(tests.schemaDown);

  it('should return RECENT_CALL_MADE', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('RECENT_CALL_MADE');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      })
      .then(function() { 
        return tests.db.query('SELECT * FROM farmradio_api.participants;');
      })
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      });
    });
  });

});

describe('Response from a participant for whom the most recent call took place more than 2 days ago', function() {

  this.timeout(6000);

  beforeEach(function() {
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp)
    .then(function() {
      var d = new Date();
      d.setMinutes(d.getMinutes() - 60*24*2);
      return tests.db.query(util.format(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('255678647268', '%s') RETURNING id;", d.toISOString()));
    })
    .then(function(results) {
      return tests.db.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('255678647268', 'NOT_REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(tests.schemaDown);

  it('should return a scheduled call with the participant\'s phone number', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('phone_number').equal(stripPrefix(response_001.subscriber_phone));
    });
  });

  it('should create a participant_registration_status_log entry in the database', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.participant_registration_status_log;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        results.rows[0].should.have.a.property('event_type').equal('REGISTRATION_CALL_SCHEDULED');
      });
    });
  });

  it('should create a new registration call in the database', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) {
        results.rowCount.should.equal(2);
      });
    });
  });

  it('should update the participant\'s registration_call_id', function() {
    return runner(response_001, function(response) {
      var registrationCall = null;
      return tests.db.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) {
        registrationCall = results.rows[1];
        registrationCall.should.be.an('object');
        return tests.db.query(util.format(
          "SELECT * FROM farmradio_api.participants WHERE phone_number='%s';", 
          stripPrefix(response_001.subscriber_phone)
        ));
      })
      .then(function(results) {
        results.rowCount.should.equal(1);
        var participant = results.rows[0];
        participant.registration_call_id.should.be.a('number');
        participant.registration_call_id.should.equal(registrationCall.id);
        return tests.db.query('SELECT * FROM farmradio_api.participant_registration_status_log;')
      })
      .then(function(results) {
        var logEntry = results.rows[0];
        logEntry.registration_call_id.should.be.a('number');
        logEntry.registration_call_id.should.equal(registrationCall.id);
      });
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

describe('Phone number with + prefix', function() {

  this.timeout(6000);

  beforeEach(function() {
    return tests.unlock()
    .then(tests.schemaDown)
    .then(tests.schemaUp);
  });

  afterEach(tests.schemaDown);

  it('should be stripped off before inserted into database', function() {
    return runner(response_001, function(response) {
      return tests.db.query('SELECT * FROM farmradio_api.participants;')
      .then(function(results) {
        var participant = results.rows[0];
        participant.phone_number.should.equal(stripPrefix(response_001.subscriber_phone));
      });
    });
  });

  it('should appear without + in response', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('phone_number').equal(stripPrefix(response_001.subscriber_phone));
    });
  });

});
