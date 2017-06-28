import * as Router from 'koa-router';

export module Api {

  let api: Router = new Router();

  api.get('/', async (ctx, next) => {
    await next();
    ctx.body = 'body';
    ctx.status = 200;
  });

  export const router = api;

}
