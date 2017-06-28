import * as Koa        from 'koa';
import * as Router     from 'koa-router';
import * as bodyparser from 'koa-bodyparser';
import * as logger     from 'koa-logger';
import * as cors       from 'kcors';

let app: Koa = new Koa();
let api: Router = new Router();

api.get('/', async (ctx, next) => {
  await next();
  ctx.body = '200 OK';
  ctx.status = 200;
});

app.use(cors())
   .use(logger())
   .use(api.routes())
   .use(api.allowedMethods())
   .use(bodyparser());

const port: number|string = process.env.PORT || 3000;
const env: string = process.env.NODE_ENV || 'development';

console.log(env);

app.listen(port);
