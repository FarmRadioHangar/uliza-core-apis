import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/organizations/3').get(test => {

  json(test);

  the(test, 'should return a single organization', response => {
    response.body
      .should.have.a.property('id')
      .that.is.a('number')
      .and.equals(3);
  });

});

Api.endpoint('/organizations/3?select=name').get(test => {

  json(test);

  the(test, 'should return a single organization', response => {
    response.body.should.not.have.a.property('id');
    response.body
      .should.have.a.property('name')
      .that.is.a('string');
  });

});



