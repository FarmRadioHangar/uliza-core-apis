import * as knex from 'knex';
import * as path from 'path';

export default (env: string) => {
  return async (ctx, next) => { 
    ctx.state.db = knex(require(path.join(__dirname, '../knexfile.js'))[env]); 
    await next(); 
  };
}
