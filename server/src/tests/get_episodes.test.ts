import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/episodes').get(test => {

  json(test);

});
