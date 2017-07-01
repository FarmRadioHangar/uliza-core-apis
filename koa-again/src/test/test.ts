import { expect }     from 'chai';
import * as supertest from 'supertest';
import * as agent     from 'supertest-koa-agent';
import app            from '../index';

describe('GET /organizations', () => {
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
