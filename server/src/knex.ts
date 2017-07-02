import { Db } from './db';

export default function() {
  return async (ctx, next) => { 
    ctx.state.db = Db.connection(); 
    await next(); 
  };
}
