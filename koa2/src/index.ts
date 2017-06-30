import 'reflect-metadata';

import * as Koa from 'koa';
import * as Router from 'koa-router';
import * as bodyparser from 'koa-bodyparser';

import { createConnection } from 'typeorm';

createConnection().then(async connection => {

  let app: Koa = new Koa();
  let router: Router = new Router();

  router.get('/organizations', async (ctx, next) => {
    await next();
    ctx.body = 'You asked for organizations';
  });

  app.use(bodyparser())
     .use(router.routes())
     .use(router.allowedMethods());

  const port: number = 3030;
  app.listen(port);

  console.log(`Koa application is up and running on port ${port}`);

}).catch(error => {
  console.log(console.log('Connection error'));
});
