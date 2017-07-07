import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/topics').get(test => {

  json(test);

});
