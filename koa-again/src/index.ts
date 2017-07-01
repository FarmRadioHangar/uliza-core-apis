import * as Koa        from 'koa';
import * as Router     from 'koa-router';
import * as bodyparser from 'koa-bodyparser';

let app    = new Koa(),
    router = new Router();

router.get('/organizations', async ctx => {
  ctx.body = { collection: [] };
});

app.use(bodyparser())
   .use(router.routes())
   .use(router.allowedMethods());

app.listen(3030);

console.log('Koa application is up and running');
