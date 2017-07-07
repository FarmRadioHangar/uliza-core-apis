import * as Router  from 'koa-router';
import * as find    from 'objection-find';
import { registerFilter } from 'objection-find';

import Country      from './models/country';
import Organization from './models/organization';

function fields(select: string|undefined): Array<string>|undefined {
  if ('string' !== typeof select) {
    return undefined;
  }
  return select.split(',');
}

function ne(propertyRef, value) {
  return {
    method: 'where',
    args: [propertyRef.fullColumnName(), '<>', value]
  };
}

export default (api: Router) => {

  /**
   * List countries.
   */
  api.get('/countries', async ctx => {
    const { select, offset, limit, ...params } = ctx.query;
    const collection = await find(Country)
      .registerFilter('ne', ne)
      .build(params)
      .skipUndefined()
      .select(fields(select))
      .offset(offset)
      .limit(limit);
    ctx.body = { collection };
  });

  /**
   * List organizations.
   */
  api.get('/organizations', async ctx => {
    const { select, offset, limit, ...params } = ctx.query;
    const collection = await find(Organization)
      .registerFilter('ne', ne)
      .build(params)
      .skipUndefined()
      .select(fields(select))
      .offset(offset)
      .limit(limit);
    ctx.body = { collection };
  });

  /**
   * Count the number of organizations matching certain criteria.
   */
  api.get('/organizations/count', async ctx => {
    const { select, offset, limit, ...params } = ctx.query;
    const results = await find(Organization)
      .registerFilter('ne', ne)
      .build(params)
      .count();
    if (1 === results.length) {
      ctx.body = { count: results[0]['count(*)'] };
    } else {
      ctx.body = { count: 0 };
    }
  });

  /**
   * Get detailed information about an organization.
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
