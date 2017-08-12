require('dotenv').config();

var assert  = require('assert');
var chai    = require('chai');
var client  = require('pg').Client;
var migrate = require('node-pg-migrate').default;
var unlock  = require('node-pg-migrate').unlockRunner;
var mocha   = require('mocha');
var request = require('supertest');
var util    = require('util');

var host = process.env.DB_HOST;
var user = process.env.DB_USER;
var pass = process.env.DB_PASS;

var server = process.env.REG_SERVICE_URL;

var conn = 'postgres://' + user + ':' + pass + '@' + host 
                         + '/postgres';
//                         + '/registration_middleware_test';

var pg = new client(conn);
pg.connect();

chai.should();
chai.use(require('chai-things'));
chai.use(require('chai-also'));
chai.use(require('chai-http'));

var config = {
  database_url: conn,
  schema: 'farmradio_api',
  migrations_schema: 'public',
  migrations_table: 'farmradio_api_registration_middleware_test_migrations',
  dir: '../../../api/db/migrations'
};

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

var response_001 = {
  data: {
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
  }
};

function runner(data, next) {
  return request(server)
  .post('/responses')
  .set('Accept', 'application/json')
  .send(data.data)
  .then(next);
}

describe('Response from new participant', function() {

  this.timeout(6000);

  beforeEach(function() { 
    return unlock(config)
    .then(schemaDown)
    .then(schemaUp); 
  });

  afterEach(schemaDown);

  it('should return a 200 OK JSON-formatted response', function() {
    return runner(response_001, function(response) {
      response.should.have.header('Content-Type', /json/);
      response.status.should.equal(200);
    });
  });

  it('should return a scheduled call with the participant\'s phone number', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('phone_number').equal(response_001.data.subscriber_phone);
    });
  });

  it('should create a voto_response_data entry in the database', function() {
    return runner(response_001, function(response) {
      return pg.query('SELECT * FROM farmradio_api.voto_response_data;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        results.rows[0].data.should.deep.equal(response_001.data);
      });
    });
  });

  it('should create a participant_registration_status_log entry in the database', function() {
    return runner(response_001, function(response) {
      return pg.query('SELECT * FROM farmradio_api.participant_registration_status_log;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        results.rows[0].should.have.a.property('event_type').equal('REGISTRATION_CALL_SCHEDULED');
      });
    });
  });

  it('should set the participant\'s registration_call_id', function() {
    return runner(response_001, function(response) {
      var registrationCall = null;
      return pg.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        registrationCall = results.rows[0];
        registrationCall.should.be.an('object');
        return pg.query(util.format(
          "SELECT * FROM farmradio_api.participants WHERE phone_number='%s';", 
          response_001.data.subscriber_phone
        ));
      })
      .then(function(results) {
        results.rowCount.should.equal(1);
        var participant = results.rows[0];
        participant.registration_call_id.should.be.a('number');
        participant.registration_call_id.should.equal(registrationCall.id);
        return pg.query('SELECT * FROM farmradio_api.participant_registration_status_log;')
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
    return unlock(config)
    .then(schemaDown)
    .then(schemaUp)
    .then(function() {
      return pg.query(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('+255678647268', '2017-07-24T18:13:51Z') RETURNING id;");
    })
    .then(function(results) {
      return pg.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('+255678647268', 'REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(schemaDown);

  it('should return PARTICIPANT_ALREADY_REGISTERED', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('PARTICIPANT_ALREADY_REGISTERED');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return pg.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      })
      .then(function() { 
        return pg.query('SELECT * FROM farmradio_api.participants;');
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
    return unlock(config)
    .then(schemaDown)
    .then(schemaUp)
    .then(function() {
      return pg.query(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status)\
         \ VALUES ('+255678647268', 'DECLINED');"
      );
    });
  });

  afterEach(schemaDown);

  it('should return REGISTRATION_DECLINED', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('REGISTRATION_DECLINED');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return pg.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(0); 
      })
      .then(function() { 
        return pg.query('SELECT * FROM farmradio_api.participants;');
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
    return unlock(config)
    .then(schemaDown)
    .then(schemaUp)
    .then(function() {
      var d = new Date();
      d.setMinutes(d.getMinutes() + 20);
      return pg.query(util.format(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('+255678647268', '%s') RETURNING id;", d.toISOString()));
    })
    .then(function(results) {
      return pg.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('+255678647268', 'NOT_REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(schemaDown);

  it('should return PRIOR_CALL_SCHEDULED', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('PRIOR_CALL_SCHEDULED');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return pg.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      })
      .then(function() { 
        return pg.query('SELECT * FROM farmradio_api.participants;');
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
    return unlock(config)
    .then(schemaDown)
    .then(schemaUp)
    .then(function() {
      var d = new Date();
      return pg.query(util.format(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('+255678647268', '%s') RETURNING id;", d.toISOString()));
    })
    .then(function(results) {
      return pg.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('+255678647268', 'NOT_REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(schemaDown);

  it('should return RECENT_CALL_MADE', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('RECENT_CALL_MADE');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return pg.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      })
      .then(function() { 
        return pg.query('SELECT * FROM farmradio_api.participants;');
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
    return unlock(config)
    .then(schemaDown)
    .then(schemaUp)
    .then(function() {
      var d = new Date();
      d.setMinutes(d.getMinutes() - 60*24*1);
      return pg.query(util.format(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('+255678647268', '%s') RETURNING id;", d.toISOString()));
    })
    .then(function(results) {
      return pg.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('+255678647268', 'NOT_REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(schemaDown);

  it('should return RECENT_CALL_MADE', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('message').equal('RECENT_CALL_MADE');
    });
  });

  it('should not create any registration call', function() {
    return runner(response_001, function(response) {
      return pg.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) { 
        results.rowCount.should.equal(1); 
      })
      .then(function() { 
        return pg.query('SELECT * FROM farmradio_api.participants;');
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
    return unlock(config)
    .then(schemaDown)
    .then(schemaUp)
    .then(function() {
      var d = new Date();
      d.setMinutes(d.getMinutes() - 60*24*2);
      return pg.query(util.format(
        "INSERT INTO farmradio_api.registration_calls\
                \ (phone_number, scheduled_time)\
         \ VALUES ('+255678647268', '%s') RETURNING id;", d.toISOString()));
    })
    .then(function(results) {
      return pg.query(util.format(
        "INSERT INTO farmradio_api.participants\
                \ (phone_number, registration_status, registration_call_id)\
         \ VALUES ('+255678647268', 'NOT_REGISTERED', %d);", results.rows[0].id));
    });
  });

  afterEach(schemaDown);

  it('should return a scheduled call with the participant\'s phone number', function() {
    return runner(response_001, function(response) {
      response.body.should.have.property('phone_number').equal(response_001.data.subscriber_phone);
    });
  });

  it('should create a participant_registration_status_log entry in the database', function() {
    return runner(response_001, function(response) {
      return pg.query('SELECT * FROM farmradio_api.participant_registration_status_log;')
      .then(function(results) {
        results.rowCount.should.equal(1);
        results.rows[0].should.have.a.property('event_type').equal('REGISTRATION_CALL_SCHEDULED');
      });
    });
  });

  it('should create a new registration call in the database', function() {
    return runner(response_001, function(response) {
      return pg.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) {
        results.rowCount.should.equal(2);
      });
    });
  });

  it('should update the participant\'s registration_call_id', function() {
    return runner(response_001, function(response) {
      var registrationCall = null;
      return pg.query('SELECT * FROM farmradio_api.registration_calls;')
      .then(function(results) {
        registrationCall = results.rows[1];
        registrationCall.should.be.an('object');
        return pg.query(util.format(
          "SELECT * FROM farmradio_api.participants WHERE phone_number='%s';", 
          response_001.data.subscriber_phone
        ));
      })
      .then(function(results) {
        results.rowCount.should.equal(1);
        var participant = results.rows[0];
        participant.registration_call_id.should.be.a('number');
        participant.registration_call_id.should.equal(registrationCall.id);
        return pg.query('SELECT * FROM farmradio_api.participant_registration_status_log;')
      })
      .then(function(results) {
        var logEntry = results.rows[0];
        logEntry.registration_call_id.should.be.a('number');
        logEntry.registration_call_id.should.equal(registrationCall.id);
      });
    });
  });

});
