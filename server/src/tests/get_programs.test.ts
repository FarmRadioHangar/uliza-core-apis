import * as chai from 'chai';
import { Api, the, assertJson200 } from './helpers';

Api.endpoint('/programs').get(test => {

  assertJson200(test);

});
