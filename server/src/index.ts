require('dotenv').config();

import * as Koa        from 'koa';
import * as bodyparser from 'koa-bodyparser';

import { Auth0 } from './auth0';
import router    from './router';
import knex      from './knex';

const env = process.env.NODE_ENV || 'development',
      app = new Koa();

if ('test' !== env) {
  app.use(Auth0.jwtCheck());
}

app.use(knex())
   .use(bodyparser())
   .use(router.routes())
   .use(router.allowedMethods())
   .listen(8080);

console.log('Server is up and running');

export default app;
