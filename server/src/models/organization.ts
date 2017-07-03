import * as objection from 'objection';

export default class Organization extends objection.Model {

  static get tableName() {
    return 'organizations';
  }

}
