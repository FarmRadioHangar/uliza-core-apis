import * as knex from 'knex';
import * as path from 'path';

const env = process.env.NODE_ENV || 'development',
      db  = knex(require(path.join(__dirname, '../knexfile.js'))[env]);

export default db;
