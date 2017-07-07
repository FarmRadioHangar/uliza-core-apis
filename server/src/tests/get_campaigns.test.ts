import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/campaigns').get(test => {

  json(test);

});
