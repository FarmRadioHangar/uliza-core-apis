import { Helpers } from './helpers';

Helpers.withSeeds('GET /organizations', (should, expect, api) => {

  it('should return JSON', async () => {
    await api 
      .get('/organizations')
      .set('Accept', 'application/json')
      .expect('Content-Type', /json/);
  });

  it('should respond with 200 OK', async () => {
    await api
      .get('/organizations')
      .expect(200);
  });

  it('should return a collection of 21 items', async () => {
    const response = await api.get('/organizations');
    response.body.should.have.property('collection');
    response.body.collection.length.should.equal(21);
  });

});

Helpers.withSeeds('GET /organizations/count', (should, expect, api) => {

  it('should return a count of 21', async () => {
    const response = await api.get('/organizations/count');
    response.body.should.have.property('count');
    response.body.count.should.equal(21);
  });

});
