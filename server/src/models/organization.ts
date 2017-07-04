import { Model } from 'objection';

export default class Organization extends Model {

  static tableName: string = 'organizations';

  static relationMappings = { };

}
