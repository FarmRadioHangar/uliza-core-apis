import { expect } from 'chai';
import * as agent from 'supertest-koa-agent';
import { Db }     from '../db';
import app        from '../index';

const db = Db.connection();

describe('GET /organizations', () => {

  before(async () => {
    await db.migrate.latest();
  });

  it('should return JSON', (done) => {
    agent(app)
      .get('/organizations')
      .set('Accept', 'application/json')
      .expect('Content-Type', /json/, done);
  });

  it('should return 200 OK', (done) => {
    agent(app)
      .get('/organizations')
      .expect(200, done);
  });

});
