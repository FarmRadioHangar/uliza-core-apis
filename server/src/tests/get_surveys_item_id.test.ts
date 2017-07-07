import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/surveys/103').get(test => {

  the(test, 'should return JSON', response => {
    response.should.have.header('Content-Type', /json/);
  });

  the(test, 'should respond with a 404 NOT FOUND response code', response => {
    response.status.should.equal(404);
  });

});
