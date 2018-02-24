var chai    = require('chai');
var request = require('supertest');
var moment  = require('moment');
var utils   = require('./utils');

chai.should();
chai.use(require('chai-things'));
chai.use(require('chai-also'));
chai.use(require('chai-http'));

var REG_SERVICE_URL      
  = process.env.REG_SERVICE_URL      || 'http://localhost:8091';
var MIN_RESCHEDULE_DELAY 
  = process.env.MIN_RESCHEDULE_DELAY || 172800;
var ULIZA_API_URL        
  = process.env.ULIZA_API_URL        || 'http://localhost:8000/api/v1';

function jsonRequest(uri) {
  return request(REG_SERVICE_URL)
  .post(uri)
  .set('Content-Type', 'application/x-www-form-urlencoded')
  .set('Accept', 'application/json')
}

describe('Uliza Join registration service', function() {

  before(function() {
    return new Promise(function(resolve, reject) {
      return utils.query('SHOW TABLES;')()
      .catch(function(err) {
        reject(Error(utils.CONNECTION_ERROR));
      })
      .then(function() {
        return jsonRequest('/');
      })
      .catch(function(err) {
        reject(Error('Failed connecting to registration server on ' + REG_SERVICE_URL));
      })
      .then(function(response) {
        return request(ULIZA_API_URL)
        .get('/')
        .set('Accept', 'application/json')
        .send();
      })
      .catch(function(err) {
        reject(Error('Failed connecting to Uliza API on ' + ULIZA_API_URL));
      })
      .then(resolve);
    });
  });

  after(utils.disconnect);

  describe('Response', function() {
  
    var registrationCallsCount;
    var participantsCount;
  
    this.timeout(4000000);
  
    beforeEach(function() {
      return utils.truncate()
      .then(utils.loadFixtures)
      .then(utils.query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) {
        registrationCallsCount = results.length;
      })
      .then(utils.query('SELECT * FROM uliza_participants;'))
      .then(function(results) {
        participantsCount = results.length;
      });
    });
  
    afterEach(function() {
      return utils.truncate();
    });
  
    describe('from a new participant', function() {
  
      var participant = {
        subscriber_phone: '256123123123',
        outgoing_call_id: '1',
        delivery_status: '3'
      };
  
      it('should return a 200 OK JSON-formatted response', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.should.have.header('Content-Type', /json/);
          response.status.should.equal(200);
        });
      });
  
      it('should return a REGISTRATION_CALL_SCHEDULED response', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body
          .should.have.property('action')
          .equal('REGISTRATION_CALL_SCHEDULED');
        });
      });
  
      it('should return details of the scheduled call with the participant\'s phone number', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body.should.have.property('registration_call');
          response.body.registration_call
          .should.have.property('phone_number')
          .equal(utils.normalizePhoneNumber(participant.subscriber_phone));
        });
      });
  
      it('should insert the participant into the database', function() {
        var sql = 'SELECT * FROM uliza_participants WHERE phone_number = ?;';
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query(sql, utils.normalizePhoneNumber(participant.subscriber_phone)))
        .then(function(results) {
          results.length.should.equal(1);
          results[0].phone_number.should.equal(
            utils.normalizePhoneNumber(participant.subscriber_phone)
          );
          results[0].registration_status.should.equal('NOT_REGISTERED');
        })
      });
  
      it('should return a valid reference to the new participant', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          var sql = 'SELECT * FROM uliza_participants WHERE id = ?;';
          return utils.query(sql, response.body.registration_call.participant)();
        })
        .then(function(results) {
          results[0].phone_number.should.equal(
            utils.normalizePhoneNumber(participant.subscriber_phone)
          );
        });
      });
  
      //it('should create a uliza_voto_webhook_log entry in the database', function() {
      //});
  
      //it('should create a participant_registration_status_log entry in the database', function() {
      //})
  
      it('should have created a registration call', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query('SELECT * FROM uliza_registration_calls;'))
        .then(function(results) {
          results.length.should.equal(registrationCallsCount + 1);
        });
      });
  
      it ('should insert the registration call into the database', function() {
        var sql = 'SELECT * FROM uliza_participants WHERE phone_number = ?;';
        var participantId;
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query(sql, utils.normalizePhoneNumber(participant.subscriber_phone)))
        .then(function(results) {
          var sql = 'SELECT * FROM uliza_registration_calls WHERE participant_id = ?;';
          participantId = results[0].id;
          return utils.query(sql, participantId)();
        })
        .then(function(results) {
          results.length.should.equal(1);
          results[0].participant_id.should.equal(participantId);
          results[0].voto_tree_id.should.equal(1);
          results[0].call_status.should.equal('SCHEDULED');
        })
      });
  
    });
  
    describe('from an already registered participant', function() {
  
      var participant = {
        subscriber_phone: '256479132865',
        outgoing_call_id: '1',
        delivery_status: '3'
      };
  
      it('should return ALREADY_REGISTERED', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body.should.have.property('action')
          .equal('NONE');
          response.body.should.have.property('reason')
          .equal('ALREADY_REGISTERED');
        });
      });
  
      it('should not create any registration call', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          return Promise.resolve()
          .then(utils.query('SELECT * FROM uliza_registration_calls;'))
          .then(function(results) {
            results.length.should.equal(registrationCallsCount);
          });
        });
      });
  
      it('should not insert a new participant into the database', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query('SELECT * FROM uliza_participants;'))
        .then(function(results) {
          results.length.should.equal(participantsCount);
        });
      });
  
    });
  
    describe('from a participant who has declined registration', function() {

      var participant = {
        subscriber_phone: '256849561372',
        outgoing_call_id: '1',
        delivery_status: '3'
      };

      it('should return REGISTRATION_DECLINED', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body.should.have.property('reason')
          .equal('REGISTRATION_DECLINED');
        });
      });

      it('should not create any registration call', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          return Promise.resolve()
          .then(utils.query('SELECT * FROM uliza_registration_calls;'))
          .then(function(results) {
            results.length.should.equal(registrationCallsCount);
          });
        });
      });
  
      it('should not insert a new participant into the database', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query('SELECT * FROM uliza_participants;'))
        .then(function(results) {
          results.length.should.equal(participantsCount);
        });
      });
 
    });
  
    describe('from a participant for whom a registration call is already due', function() {
  
      var time = moment()
      .add(600, 'seconds')
      .utc()
      .format('YYYY-MM-DD HH:mm');
  
      beforeEach(function() {
        return Promise.resolve()
        .then(utils.query(
          'UPDATE uliza_registration_calls SET schedule_time = ? WHERE id = 1;',
          [time]
        ));
      });
  
      var participant = {
        subscriber_phone: '255127386549',
        outgoing_call_id: '1',
        delivery_status: '3'
      };
  
      it('should return PRIOR_CALL_SCHEDULED', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body.should.have.property('action')
          .equal('NONE');
          response.body.should.have.property('reason')
          .equal('PRIOR_CALL_SCHEDULED');
        });
      });
  
      it('should not create any registration call', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          return Promise.resolve()
          .then(utils.query('SELECT * FROM uliza_registration_calls;'))
          .then(function(results) {
            results.length.should.equal(registrationCallsCount);
          });
        });
      });
  
      it('should not insert a new participant into the database', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query('SELECT * FROM uliza_participants;'))
        .then(function(results) {
          results.length.should.equal(participantsCount);
        });
      });
  
    });
  
    //describe('from a participant for whom a registration call took place recently', function() {
    //});
  
    describe('from a participant for whom the most recent call took place ' + MIN_RESCHEDULE_DELAY/2 + ' seconds ago', function() {
  
      var time = moment()
      .subtract(MIN_RESCHEDULE_DELAY/2, 'seconds')
      .utc()
      .format('YYYY-MM-DD HH:mm');
  
      beforeEach(function() {
        return Promise.resolve()
        .then(utils.query(
          'UPDATE uliza_registration_calls SET schedule_time = ? WHERE id = 1;',
          [time]
        ))
      });
  
      var participant = {
        subscriber_phone: '255127386549',
        outgoing_call_id: '1',
        delivery_status: '3'
      };
  
      it('should return TOO_SOON', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body.should.have.property('reason').equal('TOO_SOON');
        });
      });
  
      it('should not create any registration call', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          return Promise.resolve()
          .then(utils.query('SELECT * FROM uliza_registration_calls;'))
          .then(function(results) {
            results.length.should.equal(registrationCallsCount);
          });
        });
      });
  
      it('should not insert a new participant into the database', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query('SELECT * FROM uliza_participants;'))
        .then(function(results) {
          results.length.should.equal(participantsCount);
        });
      });
  
    });
  
    describe('from a participant for whom the most recent call took place more than ' + MIN_RESCHEDULE_DELAY + ' seconds ago', function() {
  
      var time = moment()
      .subtract(MIN_RESCHEDULE_DELAY + 3600, 'seconds')
      .utc()
      .format('YYYY-MM-DD HH:mm');
  
      beforeEach(function() {
        return Promise.resolve()
        .then(utils.query(
          'UPDATE uliza_registration_calls SET schedule_time = ? WHERE id = 1;',
          [time]
        ))
      });
  
      var participant = {
        subscriber_phone: '255127386549',
        outgoing_call_id: '1',
        delivery_status: '3'
      };
  
      it('should return details of the scheduled call with the participant\'s phone number', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body.should.have.property('registration_call');
          response.body.registration_call
          .should.have.property('phone_number')
          .equal(utils.normalizePhoneNumber(participant.subscriber_phone));
        });
      });
  
      //it('should create a participant_registration_status_log entry in the database', function() {
      //});

      it('should insert the participant into the database', function() {
        var sql = 'SELECT * FROM uliza_participants WHERE phone_number = ?;';
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query(sql, utils.normalizePhoneNumber(participant.subscriber_phone)))
        .then(function(results) {
          results.length.should.equal(1);
          results[0].phone_number.should.equal(
            utils.normalizePhoneNumber(participant.subscriber_phone)
          );
          results[0].registration_status.should.equal('NOT_REGISTERED');
        })
      });
  
    });
  
    describe('with a bad request format', function() {

      it('should return a status code 400', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send('Scooby-Doo')
        .then(function(response) { 
          response.should.have.header('Content-Type', /json/);
          response.status.should.equal(400);
        });
      });

    });

    describe('with a non-JSON request format', function() {

      it('should return a status code 400', function() {
        return request(REG_SERVICE_URL)
        .post('/responses')
        .set('Content-Type', 'application/text')
        .send('Scooby-Doo')
        .then(function(response) { 
          response.status.should.equal(400);
        });
      });

    });

    describe('already being processed in a separate response', function() {
      
      var participant = {
        subscriber_phone: '256692185743',
        outgoing_call_id: '305',
        delivery_status: '3'
      };
  
      it('should return RACE_CONDITION', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body.should.have.property('action')
          .equal('NONE');
          response.body.should.have.property('reason')
          .equal('RACE_CONDITION');
        });
      });
 
      // ...

    });
  
    describe('with a "stalled" registration call', function() {
      
      var participant = {
        subscriber_phone: '256692185743',
        outgoing_call_id: '405',
        delivery_status: '3'
      };

      it('should return a REGISTRATION_CALL_SCHEDULED response', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body
          .should.have.property('action')
          .equal('REGISTRATION_CALL_SCHEDULED');
        });
      });
  
      // ...
 
    });

    describe('Well-formed phone number', function() {

      var participant = {
        subscriber_phone: '+256123123123',
        outgoing_call_id: '1',
        delivery_status: '3'
      };

      it('should not change before inserted into database', function() {
        var sql = 'SELECT * FROM uliza_participants WHERE phone_number = ?;';
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query(sql, participant.subscriber_phone))
        .then(function(results) {
          results.length.should.equal(1);
        });
      });

    });

    describe('Phone number without + prefix', function() {

      var participant = {
        subscriber_phone: '256123123123',
        outgoing_call_id: '1',
        delivery_status: '3'
      };

      it('should be normalized before inserted into database', function() {
        var sql = 'SELECT * FROM uliza_participants WHERE phone_number = ?;';
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query(sql, utils.normalizePhoneNumber(participant.subscriber_phone)))
        .then(function(results) {
          results.length.should.equal(1);
        });
      });

      it('should appear with + in response', function() {
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(function(response) {
          response.body.registration_call.should.have.property('phone_number')
          .equal(utils.normalizePhoneNumber(participant.subscriber_phone));
        });
      });

    });

    describe('Phone number with spaces', function() {

      var participant = {
        subscriber_phone: '256 777 423 221',
        outgoing_call_id: '1',
        delivery_status: '3'
      };

      it('should be normalized before inserted into database', function() {
        var sql = 'SELECT * FROM uliza_participants WHERE phone_number = "+256777423221";';
        return jsonRequest('/responses?tree_id=1') 
        .send(utils.serialize(participant))
        .then(utils.query(sql))
        .then(function(results) {
          results.length.should.equal(1);
          results[0].phone_number.should.equal('+256777423221');
        });
      });

    });

  });

  describe('Call status update', function() {

    var registrationCallsCount;
    var participantsCount;

    this.timeout(4000000);

    beforeEach(function() {
      return utils.truncate()
      .then(utils.loadFixtures)
      .then(utils.query('SELECT * FROM uliza_registration_calls;'))
      .then(function(results) {
        registrationCallsCount = results.length;
      })
      .then(utils.query('SELECT * FROM uliza_participants;'))
      .then(function(results) {
        participantsCount = results.length;
      });
    });

    afterEach(function() {
      return utils.truncate();
    });

    describe('without delivery status field', function() {

      var update = {
        subscriber_phone: '255127386549'
      };

      it('should return a badWebhookRequest response', function() {
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(function(response) {
          response.should.have.header('Content-Type', /json/);
          response.status.should.equal(400);
          response.body.should.have.property('error')
          .equal('badWebhookRequest');
        });
      });

    });

    describe('with delivery status = "6"', function() {

      var update = {
        delivery_status: '6',
        subscriber_phone: '255127386549',
        subscriber_id: 3,
        outgoing_call_id: '2345554',
        delivery_log_id: '224445948'
      };

      it('should return a 200 OK JSON-formatted response', function() {
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(function(response) {
          response.should.have.header('Content-Type', /json/);
          response.status.should.equal(200);
        });
      });

      //it('should create a uliza_voto_webhook_log entry in the database', function() {
      //});

      it('should change the participant\'s registration_status to REGISTERED', function() {
        var sql = 'SELECT * FROM uliza_participants WHERE phone_number = ?;';
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(utils.query(sql, utils.normalizePhoneNumber(update.subscriber_phone)))
        .then(function(results) {
          results.length.should.equal(1);
          results[0].should.have.property('registration_status')
          .equal('REGISTERED');
        });
      });

      it('should return a response object with a registration_status property set to REGISTERED', function() {
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(function(response) { 
          response.body.should.have.property('data'); 
          var participant = response.body.data;
          participant.should.have.property('registration_status')
          .equal('REGISTERED');
        });
      });

      it('should have assigned registration attributes to the participant', function() {
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(request(ULIZA_API_URL)
          .get('/participants/?phone_number=' + update.subscriber_phone)
          .set('Accept', 'application/json')
          .send())
        .then(function(response) { 
          response.body.should.have.property('data'); 
          var participant = response.body.data;
          participant.should.have.property('attributes');
          participant.attributes.should.be.an('object');
          participant.attributes.should.have.property('attribute__location').equal('Arusha');
          participant.attributes.should.have.property('attribute__gender').equal('Female');
          participant.attributes.should.have.property('attribute__occupation').equal('Farmer');
          participant.attributes.should.not.have.property('attribute__shoe_size');
        });
      });

      it('should have saved the registration call interactions', function() {
        var sql = 'SELECT * FROM uliza_registration_calls JOIN uliza_participants ON uliza_participants.id = uliza_registration_calls.participant_id WHERE uliza_participants.phone_number = ?;';
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(utils.query(sql, utils.normalizePhoneNumber(update.subscriber_phone)))
        .then(function(results) {
          results.length.should.equal(1);
          results[0].should.have.property('interactions');
          results[0].interactions.should.be.a('string').that.is.not.empty;
        });
      });

    });

    describe('with delivery status = 6', function() {

      var update = {
        delivery_status: 6,
        subscriber_phone: '255127386549',
        subscriber_id: 3,
        outgoing_call_id: '2345554',
        delivery_log_id: '224445948'
      };

      it('should return a 200 OK JSON-formatted response', function() {
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(function(response) {
          response.should.have.header('Content-Type', /json/);
          response.status.should.equal(200);
        });
      });

      //it('should create a uliza_voto_webhook_log entry in the database', function() {
      //});

      it('should change the participant\'s registration_status to REGISTERED', function() {
        var sql = 'SELECT * FROM uliza_participants WHERE phone_number = ?;';
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(utils.query(sql, utils.normalizePhoneNumber(update.subscriber_phone)))
        .then(function(results) {
          results.length.should.equal(1);
          results[0].should.have.property('registration_status')
          .equal('REGISTERED');
        });
      });

      it('should change the registration call\'s call_status to COMPLETE', function() {
        var sql = 'SELECT * FROM uliza_registration_calls JOIN uliza_participants ON uliza_participants.id = uliza_registration_calls.participant_id WHERE uliza_participants.phone_number = ?;';
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(utils.query(sql, utils.normalizePhoneNumber(update.subscriber_phone)))
        .then(function(results) {
          results.length.should.equal(1);
          results[0].should.have.property('call_status')
          .equal('COMPLETE');
        });
      });

      it('should have saved the registration call interactions', function() {
        var sql = 'SELECT * FROM uliza_registration_calls JOIN uliza_participants ON uliza_participants.id = uliza_registration_calls.participant_id WHERE uliza_participants.phone_number = ?;';
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(utils.query(sql, utils.normalizePhoneNumber(update.subscriber_phone)))
        .then(function(results) {
          results.length.should.equal(1);
          results[0].should.have.property('interactions');
          results[0].interactions.should.be.a('string').that.is.not.empty;
        });
      });

    });

    describe('with delivery status = 5', function() {

      var update = {
        delivery_status: 5,
        subscriber_phone: '255127386549',
        subscriber_id: 3,
        outgoing_call_id: '2201623',
        delivery_log_id: '224445948'
      };

      it('should return a 200 OK JSON-formatted response', function() {
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(function(response) {
          response.should.have.header('Content-Type', /json/);
          response.status.should.equal(200);
        });
      });

      //it('should create a uliza_voto_webhook_log entry in the database', function() {
      //});

      it('should NOT change the participant\'s registration_status to REGISTERED', function() {
        var sql = 'SELECT * FROM uliza_participants WHERE phone_number = ?;';
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(utils.query(sql, utils.normalizePhoneNumber(update.subscriber_phone)))
        .then(function(results) {
          results.length.should.equal(1);
          results[0].should.have.property('registration_status')
          .equal('NOT_REGISTERED');
        });
      });

    });

    describe('with a non-existing subscriber', function() {

      var update = {
        delivery_status: 6,
        subscriber_phone: '257999999999',
        subscriber_id: 3,
        outgoing_call_id: '2345554',
        delivery_log_id: '224445948'
      };

      it('should return a 404 error response', function() {
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(function(response) {
          response.should.have.header('Content-Type', /json/);
          response.status.should.equal(404);
        });
      });

      it('should NOT create a new participant', function() {
        return jsonRequest('/call_status_updates') 
        .send(utils.serialize(update))
        .then(utils.query('SELECT * FROM uliza_participants;'))
        .then(function(results) {
          results.length.should.equal(participantsCount);
        });
      });

    });

    describe('with a bad request format', function() {

      it('should return a status code 400', function() {
        return jsonRequest('/call_status_updates') 
        .send('Scooby-Doo')
        .then(function(response) { 
          response.should.have.header('Content-Type', /json/);
          response.status.should.equal(400);
        });
      });

    });

    describe('with a non-JSON request format', function() {

      it('should return a status code 400', function() {
        return request(REG_SERVICE_URL)
        .post('/call_status_updates')
        .set('Content-Type', 'application/text')
        .send('Scooby-Doo')
        .then(function(response) { 
          response.status.should.equal(400);
        });
      });

    });

  });

});
