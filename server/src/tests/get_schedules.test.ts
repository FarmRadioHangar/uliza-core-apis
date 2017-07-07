import * as chai from 'chai';
import { Api, the, assertJson200 } from './helpers';

Api.endpoint('/schedules').get(test => {

  assertJson200(test);

});
