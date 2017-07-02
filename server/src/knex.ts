import { Db } from './db';

export default () => {
  return async (ctx, next) => { 
    ctx.state.db = Db.connection(); 
    await next(); 
  };
}
