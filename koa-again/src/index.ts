import * as Koa        from 'koa';
import * as Router     from 'koa-router';
import * as bodyparser from 'koa-bodyparser';
import * as knex       from 'knex';
import * as path       from 'path';

import { Auth0 } from './auth0';

const env  = process.env.NODE_ENV || 'development',
      db   = knex(require(path.join(__dirname, '../knexfile.js'))[env]);

let app    = new Koa(),
    router = new Router();

router.get('/organizations', async ctx => {
  const collection = await db('organizations');
  ctx.body = { collection };
});

router.get('/protected', async ctx => {
  ctx.body = { message: 'This API is a teapot.' };
});

if ('test' !== env) {
  app.use(Auth0.jwtCheck());
}

app.use(bodyparser())
   .use(router.routes())
   .use(router.allowedMethods())
   .listen(8080);

console.log('Server is up and running');

export default app;
