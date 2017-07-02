require('dotenv').config();

import * as Koa        from 'koa';
import * as bodyparser from 'koa-bodyparser';

import { Auth0 } from './auth0';
import router    from './router';
import knex      from './knex';

const env  = process.env.NODE_ENV || 'development',
      app  = new Koa(),
      port = process.env.PORT || 8080;

if ('test' !== env) {
  app.use(Auth0.jwtCheck());
}

app.use(knex())
   .use(bodyparser())
   .use(router.routes())
   .use(router.allowedMethods())
   .listen(port);

console.log(`Server is up and running on port ${port}`);

export default app;
