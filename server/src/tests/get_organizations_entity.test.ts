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

  the(test, 'should return an object with id, name, created_at, and updated_at properties', response => {
    response.body
      .should.have.a.property('id')
      .that.is.a('number')
      .and.also.have.a.property('name')
      .that.is.a('string')
      .and.also.have.a.property('created_at')
      .that.is.a('string')
      .and.also.have.a.property('updated_at')
      .that.is.a('string');
  });

});

Api.endpoint('/organizations/3?select=name').get(test => {

  json(test);

  the(test, 'should return a single organization with a name property', response => {
    response.body
      .should.have.a.property('name')
      .that.is.a('string');
  });

  the(test, 'should not return an object with the id property', response => {
    response.body.should.not.have.a.property('id');
  });

});
