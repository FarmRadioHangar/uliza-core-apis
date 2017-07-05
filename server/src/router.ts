import * as Router from 'koa-router';
import * as find   from 'objection-find';

import Organization from './models/organization';

export default (api: Router) => {

  api.get('/organizations', async ctx => {
    const { offset, limit, ...params } = ctx.query;
    const collection = await find(Organization)
      .build(params)
      .skipUndefined()
      .offset(offset)
      .limit(limit);
    ctx.body = { collection };
  });

  api.get('/organizations/:id', async ctx => {
    const results = await Organization.query().where('id', '=', ctx.params.id);
    if (results.length === 1) {
      ctx.body = results[0];
    } 
  });

  api.get('/organizations/count', async ctx => {
    const results = await Organization.query().count();
    ctx.body = { count: results[0]['count(*)'] };
  });

  api.get('/protected', async ctx => {
    ctx.body = { message: 'This API is a teapot.' };
  });

}
