import { expect } from 'chai';
import * as agent from 'supertest-koa-agent';
import { Db }     from '../db';
import app        from '../index';

const db = Db.connection();

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

  it('should return 200 OK', async () => {
    agent(app)
      .get('/organizations')
      .expect(200);
  });

});
