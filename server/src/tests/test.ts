import { expect } from 'chai';
import * as agent from 'supertest-koa-agent';
import * as knex  from 'knex';
import * as path  from 'path';
import app        from '../index';

const db = knex(require(path.join(__dirname, '../../knexfile.js')).test);

describe('GET /organizations', () => {

  before(() => {
    db.migrate.latest();
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
