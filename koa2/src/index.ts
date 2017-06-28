import * as Koa        from 'koa';
import * as bodyparser from 'koa-bodyparser';
import * as logger     from 'koa-logger';
import * as cors       from 'kcors';

let app: Koa = new Koa();

app.use(cors())
   .use(logger())
   .use(bodyparser());

app.listen(3000);
