import * as chai  from 'chai';
import * as agent from 'supertest-koa-agent';
import { Db }     from '../db';
import app        from '../index';

const should = chai.should(),
      expect = chai.expect,
      db = Db.connection();

describe('GET /organizations', () => {

  before(async () => {
    await db.migrate.latest();
    await db.seed.run();
  });

  it('should return JSON', async () => {
    await agent(app)
      .get('/organizations')
      .set('Accept', 'application/json')
      .expect('Content-Type', /json/);
  });

  it('should respond with 200 OK', async () => {
    await agent(app)
      .get('/organizations')
      .expect(200);
  });

  it('should return a collection of 3 items', async () => {
    const response = await agent(app).get('/organizations');
    response.body.should.have.property('collection');
    response.body.collection.length.should.equal(3);
  });

});
