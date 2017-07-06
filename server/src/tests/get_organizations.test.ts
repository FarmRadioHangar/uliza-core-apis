import * as chai from 'chai';
import { Api }   from './helpers';

Api.test.endpoint('/organizations').get(result => {

  it('should return JSON', async () => {
    await result.expect('Content-Type', /json/);
  });

  it('should respond with 200 OK', async () => {
    await result.expect(200);
  });

  it('should return a collection of 21 items', async () => {
    const response = await result;
    response.body.should.have.property('collection');
    response.body.collection.length.should.equal(21);
  });

});

Api.test.endpoint('/organizations?id:in=3,7,11,14').get(result => {

  it('should return a collection of 4 items', async () => {
    const response = await result;
    response.body.collection.length.should.equal(4);
  });

});

Api.test.endpoint('/organizations?select=name').get(result => {

  it('should return a collection of items with only a \'name\' property', async () => {
    const response = await result;
    response.body.should.have.property('collection');
    response.body.collection.should.be.an('array');
    response.body.collection.length.should.equal(21);
    response.body.collection.should.all.not.have.property('id');
    response.body.collection.should.all.have.property('name');
  });

});

Api.test.endpoint('/organizations?select=id,name').get(result => {

  it('should return a collection of items with both \'id\' and \'name\' properties', async () => {
    const response = await result;
    response.body.should.have.property('collection');
    response.body.collection.length.should.equal(21);
    response.body.collection.forEach(item => {
      const keys: Array<string> = Object.keys(item);
      keys.length.should.equal(2);
      keys[0].should.equal('id');
      keys[1].should.equal('name');
    });
  });

});

Api.test.endpoint('/organizations?id:gt=15').get(result => {

  it('should return a collection of 6 items', async () => {
    const response = await result;
    response.body.collection.length.should.equal(6);
  });

});
