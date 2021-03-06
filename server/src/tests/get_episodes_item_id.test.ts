import * as chai from 'chai';
import { Api, the, assertJson200 } from './helpers';

Api.endpoint('/episodes/103').get(test => {

  the(test, 'should return JSON', response => {
    response.should.have.header('Content-Type', /json/);
  });

  the(test, 'should respond with a 404 NOT FOUND response code', response => {
    response.status.should.equal(404);
  });

});
