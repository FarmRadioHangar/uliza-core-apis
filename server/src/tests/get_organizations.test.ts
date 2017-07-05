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

Helpers.withSeeds('GET /organizations?id:in=3,7,11,14', (should, expect, api) => {

  it('should return a collection of 4 items', async () => {
    const response = await api.get('/organizations?id:in=3,7,11,14');
    response.body.collection.length.should.equal(4);
  });

});

Helpers.withSeeds('GET /organizations?id:gt=15', (should, expect, api) => {

  it('should return a collection of 6 items', async () => {
    const response = await api.get('/organizations?id:gt=15');
    response.body.collection.length.should.equal(6);
  });

});

Helpers.withSeeds('GET /organizations/count', (should, expect, api) => {

  it('should respond with 200 OK', async () => {
    await api
      .get('/organizations/count')
      .expect(200);
  });

  it('should return a count of 21', async () => {
    const response = await api.get('/organizations/count');
    response.body.should.have.property('count');
    response.body.count.should.equal(21);
  });

});
