import * as knex from 'knex';
import * as path from 'path';

export namespace Db {

  const env = process.env.NODE_ENV || 'development',
        db  = knex(require(path.join(__dirname, '../knexfile.js'))[env]);

  export function connection() {
    return db;
  };

}
