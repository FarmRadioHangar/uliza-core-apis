import { Model } from 'objection';

export default class Organization extends Model {

  static get tableName() {
    return 'organizations';
  }

}
