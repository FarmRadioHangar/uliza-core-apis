import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/organizations').get(test => {

  json(test);

  the(test, 'should return a collection of 21 items', response => {
    response.body.should.have.a.property('collection');
    response.body.collection.should.be.an('array');
    response.body.collection.length.should.equal(21);
  });

});

Api.endpoint('/organizations?id:in=3,7,11,14').get(test => {

  json(test);

  the(test, 'should return a collection of 4 items', response => {
    response.body.collection.should.be.an('array');
    response.body.collection.length.should.equal(4);
  });

});

Api.endpoint('/organizations?select=name').get(test => {

  json(test);

  the(test, 'should return a collection of items with only a \'name\' property', response => {
    response.body.should.have.a.property('collection');
    response.body.collection.should.be.an('array');
    response.body.collection.length.should.equal(21);
    response.body.collection.should.all.not.have.property('id');
    response.body.collection.should.all.have.property('name');
  });

});

Api.endpoint('/organizations?select=id,name').get(test => {

  json(test);

  the(test, 'should return a collection of items with both \'id\' and \'name\' properties', response => {
    response.body.should.have.a.property('collection');
    response.body.collection.should.be.an('array');
    response.body.collection.length.should.equal(21);
    response.body.collection.should.all.have.a.property('id');
    response.body.collection.should.all.have.a.property('name');
    response.body.collection.should.all.not.have.a.property('created_at');
    response.body.collection.should.all.not.have.a.property('updated_at');
  });

});

Api.endpoint('/organizations?id:gt=15').get(test => {

  json(test);

  the(test, 'should return a collection of 6 items', response => {
    response.body.collection.should.be.an('array');
    response.body.collection.length.should.equal(6);
  });

});
