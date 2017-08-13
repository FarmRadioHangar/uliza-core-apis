var util  = require('util');
var tests = require('./utils');

var call_status_update_001 = {
  subscriber_id: "1",
  subscriber_phone: "255678647268",
  delivery_status: "6",
  registered: "true"
};

var call_status_update_002 = {
  subscriber_id: 1,
  subscriber_phone: "255678647268",
  delivery_status: 6,
  registered: true
};

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

  it('should accept request objects with non-string values', function() {
    return runner(call_status_update_001, function(response) {
      response.should.have.header('Content-Type', /json/);
      response.status.should.equal(200);
    });
  });

});
