import * as Router from 'koa-router';
import * as find   from 'objection-find';

import Organization from './models/organization';

function fields(select: string|undefined): Array<string>|undefined {
  if ('string' !== typeof select) {
    return undefined;
  }
  return select.split(',');
}

export default (api: Router) => {

  /**
   * GET /organizations
   */
  api.get('/organizations', async ctx => {
    const { select, offset, limit, ...params } = ctx.query;
    const collection = await find(Organization)
      .build(params)
      .skipUndefined()
      .select(fields(select))
      .offset(offset)
      .limit(limit);
    ctx.body = { collection };
  });

  /**
   * GET /organizations/count
   */
  api.get('/organizations/count', async ctx => {
    const { select, offset, limit, ...params } = ctx.query;
    const results = await find(Organization)
      .build(params)
      .count();
    if (1 === results.length) {
      ctx.body = { count: results[0]['count(*)'] };
    } else {
      ctx.body = { count: 0 };
    }
  });

  /**
   * GET /organizations/:id
   */
  api.get('/organizations/:id', async ctx => {
    const { select } = ctx.query;
    const results = await Organization
      .query()
      .where('id', '=', ctx.params.id)
      .skipUndefined()
      .select(fields(select));
    if (1 === results.length) {
      ctx.body = results[0];
    } 
  });

  api.get('/protected', async ctx => {
    ctx.body = { message: 'This API is a teapot.' };
  });

}
