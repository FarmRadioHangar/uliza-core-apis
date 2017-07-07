import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/countries').get(test => {

  json(test);

});
