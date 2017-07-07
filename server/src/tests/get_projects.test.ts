import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/projects').get(test => {

  json(test);

});
