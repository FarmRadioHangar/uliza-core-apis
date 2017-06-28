import * as Koa        from 'koa';
import * as bodyparser from 'koa-bodyparser';
import * as logger     from 'koa-logger';

let app: Koa = new Koa();

app.use(logger())
   .use(bodyparser());

app.listen(3000);
