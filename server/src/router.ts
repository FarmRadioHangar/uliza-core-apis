import * as Router from 'koa-router';

import { Organization } from './models/organization';

let router = new Router();

router.get('/organizations', async ctx => {
  const collection = await Organization.all();
  ctx.body = { collection };
});

router.get('/protected', async ctx => {
  ctx.body = { message: 'This API is a teapot.' };
});

export default router;
