import * as agent from 'supertest-koa-agent';
import * as chai  from 'chai';
import db         from '../db';
import app        from '../index';

export function json(test) {

  it('should return JSON', async () => {
    const response = await test;
    response.should.have.header('Content-Type', /json/);
  });

  it('should respond with 200 OK', async () => {
    const response = await test;
    response.status.should.equal(200);
  });

}

export const the = (t, h, e) => it(h, () => t.then(e));

export module Api {

  chai.should();
  chai.use(require('chai-things'));
  chai.use(require('chai-also'));
  chai.use(require('chai-http'));

  export function endpoint(url) {

    beforeEach(async () => {
      await db.migrate.rollback();
      await db.migrate.latest();
      await db.seed.run();
    });

    afterEach(async () => {
      await db.migrate.rollback();
    });

    return {

      get(f) { 
        describe(`GET ${url}`, f.bind(null, agent(app)
          .get(url)
          .set('Accept', 'application/json'))); 
      },

      post(f) { 
        describe(`POST ${url}`, f.bind(null, agent(app)
          .post(url)
          .set('Accept', 'application/json'))); 
      },

      put(f) { 
        describe(`PUT ${url}`, f.bind(null, agent(app)
          .put(url)
          .set('Accept', 'application/json'))); 
      },

      head(f) { 
        describe(`HEAD ${url}`, f.bind(null, agent(app)
          .head(url)
          .set('Accept', 'application/json'))); 
      },

      patch(f) { 
        describe(`PATCH ${url}`, f.bind(null, agent(app)
          .patch(url)
          .set('Accept', 'application/json'))); 
      },

      delete(f) { 
        describe(`DELETE ${url}`, f.bind(null, agent(app)
          .delete(url)
          .set('Accept', 'application/json'))); 
      }

    };

  }

}
