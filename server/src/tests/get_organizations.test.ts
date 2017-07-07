import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/organizations').get(test => {

  json(test);

  the(test, 'should return a collection of 21 items', response => {
    response.body
      .should.have.a.property('collection')
      .that.is.an('array')
      .and.has.a.lengthOf(21);
  });

});

Api.endpoint('/organizations?id:in=3,7,11,14').get(test => {

  json(test);

  the(test, 'should return a collection of 4 items', response => {
    response.body.collection
      .should.be.an('array')
      .and.have.a.lengthOf(4);
  });

});

Api.endpoint('/organizations?select=name').get(test => {

  json(test);

  the(test, 'should return a collection of items with only a \'name\' property', response => {
    response.body
      .should.have.a.property('collection')
      .that.is.an('array')
      .and.has.a.lengthOf(21);
    response.body.collection
      .should.all.have.property('name');
  });

  the(test, 'should not return any items with an \'id\' property', response => {
    response.body.collection
      .should.all.not.have.a.property('id');
  });

});

Api.endpoint('/organizations?select=id,name').get(test => {

  json(test);

  the(test, 'should return a collection of items with both \'id\' and \'name\' properties', response => {
    response.body
      .should.have.a.property('collection')
      .that.is.an('array')
      .and.has.a.lengthOf(21);
    response.body.collection
      .should.all.have.a.property('id')
      .and.all.have.a.property('name');
  });

  the(test, 'should not return any items with a \'created_at\' or \'updated_at\' property', response => {
    const { collection } = response.body;
    collection.should.all.not.have.a.property('created_at');
    collection.should.all.not.have.a.property('updated_at');
  });

});

Api.endpoint('/organizations?id:gt=15').get(test => {

  json(test);

  the(test, 'should return a collection of 6 items', response => {
    response.body.collection
      .should.be.an('array')
      .and.have.a.lengthOf(6);
  });

});

Api.endpoint('/organizations?offset=0&limit=4').get(test => {

  the(test, 'should return a collection of 4 items, starting from id 1', response => {
    response.body.should.have.a.property('collection').that.is.an('array').and.has.a.lengthOf(4);
    response.body.collection[0].id.should.equal(1);
  });

});

Api.endpoint('/organizations?offset=4&limit=4').get(test => {

  the(test, 'should return a collection of 4 items, starting from id 5', response => {
    response.body.should.have.a.property('collection').that.is.an('array').and.has.a.lengthOf(4);
    response.body.collection[0].id.should.equal(5);
  });

});

Api.endpoint('/organizations?offset=8&limit=4').get(test => {

  the(test, 'should return a collection of 4 items, starting from id 9', response => {
    response.body.should.have.a.property('collection').that.is.an('array').and.has.a.lengthOf(4);
    response.body.collection[0].id.should.equal(9);
  });

});

Api.endpoint('/organizations?offset=12&limit=4').get(test => {

  the(test, 'should return a collection of 4 items, starting from id 13', response => {
    response.body.should.have.a.property('collection').that.is.an('array').and.has.a.lengthOf(4);
    response.body.collection[0].id.should.equal(13);
  });

});

Api.endpoint('/organizations?offset=16&limit=4').get(test => {

  the(test, 'should return a collection of 4 items, starting from id 17', response => {
    response.body.should.have.a.property('collection').that.is.an('array').and.has.a.lengthOf(4);
    response.body.collection[0].id.should.equal(17);
  });

});

Api.endpoint('/organizations?offset=20&limit=4').get(test => {

  the(test, 'should return a collection of 1 item, with id = 21', response => {
    response.body.should.have.a.property('collection').that.is.an('array').and.has.a.lengthOf(1);
    response.body.collection[0].id.should.equal(21);
  });

});

Api.endpoint('/organizations?id:ne=3').get(test => {

  the(test, 'should return a collection of 20 organizations, not containing id = 3', response => {
    response.body
      .should.have.a.property('collection')
      .that.is.an('array')
      .and.has.a.lengthOf(20);
    response.body.collection.should.contain.a.thing.with.property('id', 1)
      .and.contain.a.thing.with.property('id', 2)
      .and.contain.a.thing.with.property('id', 4);
    response.body.collection.should.not.contain.a.thing.with.property('id', 3);
  });

});

Api.endpoint('/organizations?id:ne=3&limit=4').get(test => {

  the(test, 'should return a collection of 4 organizations, not containing id = 3', response => {
    response.body.collection.length.should.equal(4);
    response.body.collection.should.contain.a.thing.with.property('id', 1)
      .and.contain.a.thing.with.property('id', 2)
      .and.contain.a.thing.with.property('id', 4)
      .and.contain.a.thing.with.property('id', 5);
    response.body.collection.should.not.contain.a.thing.with.property('id', 3);
  });

});
