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
var VOTO_API_KEY    = process.env.VOTO_API_KEY || 'xxxxxxxxxxxxxxxxxxxxxxxxx'

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
  return request(REG_SERVICE_URL)
  .post('/responses')
  .set('Content-Type', 'application/x-www-form-urlencoded')
  .set('Accept', 'application/json')
  .send(serialize(data));
}

describe('/responses', function() {

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
    .then(query('TRUNCATE TABLE uliza_voto_survey_registration_tree;'))
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
      return request(ULIZA_API_URL)
      .post('/voto_survey_registration_tree')
      .set('Content-Type', 'application/json')
      .set('Accept', 'application/json')
      .send({
        voto_survey_id: 89324,
        voto_tree_id: 19278
      })
    })
    .catch(console.error);
  });

  afterEach(function() { 
    return Promise.resolve()
    .then(function() {
      self._db.end();
    });
  });

  describe('Response from new participant', function() {
  
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
  
    it('should create a uliza_voto_webhook_log entry in the database', function() {
      return runner()
      .then(query('SELECT * FROM uliza_voto_webhook_log;'))
      .then(function(results) {
        results.length.should.equal(1);
        var row = qs.parse(results[0].data);
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
  
    it('should set the participant\'s registration_call_id', function() {
      var registrationCall = null;
      return runner()
      .then(query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) {
        results.length.should.equal(1);
        registrationCall = results[0];
        registrationCall.should.be.an('object');
      })
      .then(query(util.format(
        'SELECT * FROM uliza_participants WHERE phone_number=\'%s\';', 
        stripPrefix(data.subscriber_phone)
      )))
      .then(function(results) {
        results.length.should.equal(1);
        var participant = results[0];
        participant.registration_call_id.should.be.a('number');
        participant.registration_call_id.should.equal(registrationCall.id);
      })
      .then(query('SELECT * FROM uliza_participant_registration_status_log;'))
      .then(function(results) {
        var logEntry = results[0];
        logEntry.registration_call_id.should.be.a('number');
        logEntry.registration_call_id.should.equal(registrationCall.id);
      })
    });
  
    it('should schedule a call with VOTO', function() {
      var votoId = null;
      return runner()
      .then(query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) {
        votoId = results[0].voto_id;
        return votoId;
      })
      .then(function(id) {
        return request(VOTO_API_URL)
        .get('/outgoing_calls/' + id + '?api_key=' + VOTO_API_KEY)
        .set('Content-Type', 'application/json')
        .send();
      })
      .then(function(response) {
        response.status.should.equal(200);
        response.body.should.have.property('data');
        response.body.data.should.have.property('outgoing_call');
        var call = response.body.data.outgoing_call;
        call.id.should.equal(String(votoId));
      });
    });

  });
  
  describe('Response from an already registered participant', function() {
  
    beforeEach(function() {
      return Promise.resolve()
      .then(query('INSERT INTO uliza_registration_calls (phone_number, scheduled_time, created_at, voto_id) VALUES (\'255678647268\', \'2017-07-24 18:13:51\', \'2017-07-24 18:03:51\', NULL);'))
      .then(query('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'REGISTERED\', LAST_INSERT_ID(), \'2017-07-24 18:04:51\');'));
    });

    it('should return ALREADY_REGISTERED', function() {
      return runner()
      .then(function(response) {
        response.body.should.have.property('reason').equal('ALREADY_REGISTERED');
      });
    });
  
    it('should not create any registration call', function() {
      return runner()
      .then(function(response) {
        return Promise.resolve()
        .then(query('SELECT * FROM uliza_registration_calls;'))
        .then(function(results) { 
          results.length.should.equal(1); 
        })
        .then(query('SELECT * FROM uliza_participants;'))
        .then(function(results) { 
          results.length.should.equal(1); 
        });
      });
    });
  
  });
  
  describe('Response from a participant who has declided to be registered', function() {
  
    beforeEach(function() {
      return Promise.resolve()
      .then(query('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'DECLINED\', NULL, \'2017-07-24 18:03:51\');'));
    });
  
    it('should return REGISTRATION_DECLINED', function() {
      return runner()
      .then(function(response) {
        response.body.should.have.property('reason').equal('REGISTRATION_DECLINED');
      });
    });
  
    it('should not create any registration call', function() {
      return runner()
      .then(function(response) {
        return Promise.resolve()
        .then(query('SELECT * FROM uliza_registration_calls;'))
        .then(function(results) { 
          results.length.should.equal(0); 
        })
        .then(query('SELECT * FROM uliza_participants;'))
        .then(function(results) { 
          results.length.should.equal(1); 
        });
      });
    });
  
  });
  
  describe('Response from a participant for whom a registration call is already due', function() {
  
    beforeEach(function() {
      var d = new Date();
      var createdAt = toDateString(d);
      d.setMinutes(d.getMinutes() + 20);
      var scheduledAt = toDateString(d);
      return Promise.resolve()
      .then(query(util.format('INSERT INTO uliza_registration_calls (phone_number, created_at, scheduled_time, voto_id) VALUES (\'255678647268\', \'%s\', \'%s\', NULL);', createdAt, scheduledAt)))
      .then(query(util.format('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'NOT_REGISTERED\', LAST_INSERT_ID(), \'%s\');', createdAt)));
    });
  
    it('should return PRIOR_CALL_SCHEDULED', function() {
      return runner()
      .then(function(response) {
        response.body.should.have.property('reason').equal('PRIOR_CALL_SCHEDULED');
      });
    });
  
    it('should not create any registration call', function() {
      return runner()
      .then(function(response) {
        return Promise.resolve()
        .then(query('SELECT * FROM uliza_registration_calls;'))
        .then(function(results) {
          results.length.should.equal(1); 
        })
        .then(query('SELECT * FROM uliza_participants;'))
        .then(function(results) {
          results.length.should.equal(1); 
        });
      });
    });
  
  });
  
  describe('Response from a participant for whom a registration call happened recently', function() {
  
    beforeEach(function() {
      var d = new Date();
      var createdAt = toDateString(d);
      return Promise.resolve()
      .then(query(util.format('INSERT INTO uliza_registration_calls (phone_number, created_at, scheduled_time, voto_id) VALUES (\'255678647268\', \'%s\', \'%s\', NULL);', createdAt, createdAt)))
      .then(query(util.format('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'NOT_REGISTERED\', LAST_INSERT_ID(), \'%s\');', createdAt)));
    });
  
    it('should return TOO_SOON', function() {
      return runner()
      .then(function(response) {
        response.body.should.have.property('reason').equal('TOO_SOON');
      });
    });
  
    it('should not create any registration call', function() {
      return runner()
      .then(function(response) {
        return Promise.resolve()
        .then(query('SELECT * FROM uliza_registration_calls;'))
        .then(function(results) {
          results.length.should.equal(1); 
        })
        .then(query('SELECT * FROM uliza_participants;'))
        .then(function(results) {
          results.length.should.equal(1); 
        })
      });
    });
  
  });
  
  describe('Response from a participant for whom the most recent call took place 1 day ago', function() {
  
    beforeEach(function() {
      var d = new Date();
      var createdAt = toDateString(d);
      d.setMinutes(d.getMinutes() - 60*24*1);
      var scheduledAt = toDateString(d);
      return Promise.resolve()
      .then(query(util.format('INSERT INTO uliza_registration_calls (phone_number, created_at, scheduled_time, voto_id) VALUES (\'255678647268\', \'%s\', \'%s\', NULL);', createdAt, scheduledAt)))
      .then(query(util.format('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'NOT_REGISTERED\', LAST_INSERT_ID(), \'%s\');', createdAt)));
    });
  
    it('should return TOO_SOON', function() {
      return runner()
      .then(function(response) {
        response.body.should.have.property('reason').equal('TOO_SOON');
      });
    });
  
    it('should not create any registration call', function() {
      return runner()
      .then(function(response) {
        return Promise.resolve()
        .then(query('SELECT * FROM uliza_registration_calls;'))
        .then(function(results) {
          results.length.should.equal(1); 
        })
        .then(query('SELECT * FROM uliza_participants;'))
        .then(function(results) {
          results.length.should.equal(1); 
        })
      });
    });
  
  });
  
  describe('Response from a participant for whom the most recent call took place more than 2 days ago', function() {
  
    beforeEach(function() {
      var d = new Date();
      var createdAt = toDateString(d);
      d.setMinutes(d.getMinutes() - 60*24*2);
      var scheduledAt = toDateString(d);
      return Promise.resolve()
      .then(query(util.format('INSERT INTO uliza_registration_calls (phone_number, created_at, scheduled_time, voto_id) VALUES (\'255678647268\', \'%s\', \'%s\', NULL);', createdAt, scheduledAt)))
      .then(query(util.format('INSERT INTO uliza_participants (phone_number, registration_status, registration_call_id, created_at) VALUES (\'255678647268\', \'NOT_REGISTERED\', LAST_INSERT_ID(), \'%s\');', createdAt)));
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
        .then(query('SELECT * FROM uliza_participant_registration_status_log;'))
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
        .then(query('SELECT * FROM uliza_registration_calls;'))
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
        .then(query('SELECT * FROM uliza_registration_calls;'))
        .then(function(results) {
          registrationCall = results[1];
          registrationCall.should.be.an('object');
          return ;
        })
        .then(query(util.format(
          'SELECT * FROM uliza_participants WHERE phone_number=\'%s\';', 
          stripPrefix(data.subscriber_phone)
        )))
        .then(function(results) {
          results.length.should.equal(1);
          var participant = results[0];
          participant.registration_call_id.should.be.a('number');
          participant.registration_call_id.should.equal(registrationCall.id);
        })
        .then(query('SELECT * FROM uliza_participant_registration_status_log;'))
        .then(function(results) {
          var logEntry = results[0];
          logEntry.registration_call_id.should.be.a('number');
          logEntry.registration_call_id.should.equal(registrationCall.id);
        });
      });
    });
  
  });
  
  describe('Response belonging to survey with no associated registration tree', function() {
  
  });

  describe('Bad request format', function() {
  
    it('should return a status code 400', function() {
      return request(REG_SERVICE_URL)
      .post('/responses')
      .set('Content-Type', 'application/x-www-form-urlencoded')
      .set('Accept', 'application/json')
      .send({})
      .then(function(response) {
        response.should.have.header('Content-Type', /json/);
        response.status.should.equal(400);
      });
    });
  
  });
  
  describe('Phone number with + prefix', function() {
  
    it('should be normalized before inserted into database', function() {
      return runner()
      .then(function(response) {
        return Promise.resolve()
        .then(query('SELECT * FROM uliza_participants;'))
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

});
