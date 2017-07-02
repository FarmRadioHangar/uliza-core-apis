import { Db } from '../db';

export namespace Organization {

  const db = Db.connection();

  export async function all() {
    return db('organizations');
  }

}
