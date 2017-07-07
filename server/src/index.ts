require('dotenv').config();

import * as Koa        from 'koa';
import * as Router     from 'koa-router';
import * as error      from 'koa-json-error';
import * as bodyparser from 'koa-bodyparser';
import { Model }       from 'objection';
import { Auth0 }       from './auth0';
import setup           from './router';
import db              from './db';

const env    = process.env.NODE_ENV || 'development',
      app    = new Koa(),
      router = new Router(),
      port   = process.env.PORT || 8080;

Model.knex(db);
setup(router);

app//.use(Auth0.jwtCheck().unless(() => 'test' === env))
   .use(bodyparser())
   .use(error(err => { return { message: err.message, status: err.status }; }))
   .use(router.routes())
   .use(router.allowedMethods())
   .listen(port)
   .on('listening', () => { 
     console.log(`Server is up and running on port ${port}`);
   });

export default app;
