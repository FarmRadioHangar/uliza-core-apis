import * as Router from 'koa-router';

let router = new Router();

router.get('/organizations', async ctx => {
  const collection = await ctx.state.db('organizations');
  ctx.body = { collection };
});

router.get('/protected', async ctx => {
  ctx.body = { message: 'This API is a teapot.' };
});

export default router;
