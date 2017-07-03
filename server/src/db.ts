import * as knex from 'knex';
import * as path from 'path';
import { Model } from 'objection';

export module Db {

  const env = process.env.NODE_ENV || 'development',
        db  = knex(require(path.join(__dirname, '../knexfile.js'))[env]);

  Model.knex(db);

  export function connection() {
    return db;
  };

}
