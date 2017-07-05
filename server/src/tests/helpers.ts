import * as agent from 'supertest-koa-agent';
import * as chai  from 'chai';
import db         from '../db';
import app        from '../index';

export module Helpers {

  export function withSeeds(what, callback) {

    beforeEach(async () => {
      await db.migrate.rollback();
      await db.migrate.latest();
      await db.seed.run();
    });

    afterEach(async () => {
      await db.migrate.rollback();
    });

    describe(what, callback.bind(null, chai.should(), chai.expect, agent(app)));

  }

}
