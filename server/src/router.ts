import * as Router from 'koa-router';
import * as find   from 'objection-find';

import Organization from './models/organization';

let router = new Router();

router.get('/organizations', async ctx => {
  const collection = await find(Organization).build(ctx.query);
  ctx.body = { collection };
});

router.get('/organizations/count', async ctx => {
  const result = await Organization.query().count();
  ctx.body = { count: result[0]['count(*)'] };
});

router.get('/protected', async ctx => {
  ctx.body = { message: 'This API is a teapot.' };
});

export default router;
