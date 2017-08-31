require('dotenv').config();

var assert   = require('assert');
var chai     = require('chai');
var mocha    = require('mocha');
var request  = require('supertest');

chai.should();
chai.use(require('chai-things'));
chai.use(require('chai-also'));
chai.use(require('chai-http'));

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

describe('Response from new participant', function() {

  this.timeout(6000);

  it('should do stuff', function() {

    return request('http://0.0.0.0:3034') // process.env.REG_SERVICE_URL)
    .post('/responses')
    .set('Accept', 'application/json')
    .send(data)
    .then(function(r) { 
      console.log(r); 
    });

  });

});


