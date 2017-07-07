import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/programs').get(test => {

  json(test);

});
