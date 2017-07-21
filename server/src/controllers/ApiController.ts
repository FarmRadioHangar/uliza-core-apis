import { Model } from 'objection';
import * as find from 'objection-find';

export default class ApiController {

  private model: typeof Model;

  constructor(model: typeof Model) {
    this.model = model;
    this.collection = this.collection.bind(this);
    this.count = this.count.bind(this);
    this.findOne = this.findOne.bind(this);
  }

  public async collection(ctx) {
    const { select, offset, limit, ...params } = ctx.query;
    const collection = await find(this.model)
      .registerFilter('ne', this.ne)
      .build(params)
      .skipUndefined()
      .select(this.fields(select))
      .offset(offset)
      .limit(limit);
    ctx.body = { collection };
  }

  public async count(ctx) {
    const { select, offset, limit, ...params } = ctx.query;
    const results = await find(this.model)
      .registerFilter('ne', this.ne)
      .build(params)
      .count();
    if (1 === results.length) {
      ctx.body = { count: results[0]['count(*)'] };
    } else {
      ctx.body = { count: 0 };
    }
  }

  public async findOne(ctx) {
    const { select } = ctx.query;
    const { id } = ctx.params;
    const results = await this.model 
      .query()
      .where('id', '=', id)
      .skipUndefined()
      .select(this.fields(select));
    if (1 === results.length) {
      ctx.body = results[0];
    } 
  }

  /**
   * @private
   */
  private ne(propertyRef, value) {
    return {
      method: 'where',
      args: [propertyRef.fullColumnName(), '<>', value]
    };
  }

  /**
   * @private
   */
  private fields(select: string|undefined): Array<string>|undefined {
    if ('string' !== typeof select) {
      return undefined;
    }
    return select.split(',');
  }

}
