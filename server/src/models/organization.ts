import { Db } from '../db';

export namespace Organization {

  export async function all() {
    return ctx.state.db('organizations');
  }

}
