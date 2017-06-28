import * as Koa        from 'koa';
import * as bodyparser from 'koa-bodyparser';
import * as logger     from 'koa-logger';
import * as cors       from 'kcors';

import { Api } from './api';

let app: Koa = new Koa();

app.use(cors())
   .use(logger())
   .use(Api.router.routes())
   .use(Api.router.allowedMethods())
   .use(bodyparser());

const port: number|string = process.env.PORT || 3000;
const env: string = process.env.NODE_ENV || 'development';

console.log(env);

app.listen(port);
