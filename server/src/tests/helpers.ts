import * as agent from 'supertest-koa-agent';
import * as chai  from 'chai';
import { Db }     from '../db';
import app        from '../index';

const db = Db.connection();

export module Helpers {

  export const { withSeeds } = {

    withSeeds(what, callback) {

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

  };

}
