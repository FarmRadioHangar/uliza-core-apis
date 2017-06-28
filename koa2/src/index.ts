import * as Koa        from 'koa';
import * as bodyparser from 'koa-bodyparser';
import * as logger     from 'koa-logger';
import * as cors       from 'kcors';

import api from './api';

let app: Koa = new Koa();

app.use(cors())
   .use(logger())
   .use(api.routes())
   .use(api.allowedMethods())
   .use(bodyparser());

app.listen(3000);
