import * as chai  from 'chai';
import * as agent from 'supertest-koa-agent';
import { Db }     from '../db';
import app        from '../index';

const should = chai.should(),
      expect = chai.expect,
      db  = Db.connection(),
      api = agent(app);

describe('GET /organizations', () => {

  beforeEach(async () => {
    await db.migrate.rollback();
    await db.migrate.latest();
    await db.seed.run();
  });

  afterEach(async () => {
    await db.migrate.rollback();
  });

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
