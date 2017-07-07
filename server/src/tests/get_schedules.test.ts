import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/schedules').get(test => {

  json(test);

});
