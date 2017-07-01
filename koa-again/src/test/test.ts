import { expect }     from 'chai';
import * as supertest from 'supertest';
import server         from '../index';

describe('GET /organizations', () => {
  it('should return JSON', (done) => {
    supertest(server)
      .get('/organizations')
      .set('Accept', 'application/json')
      .expect('Content-Type', /json/, done);
  });
  it('should return 200 OK', (done) => {
    supertest(server)
      .get('/organizations')
      .expect(200, done);
  });
});
