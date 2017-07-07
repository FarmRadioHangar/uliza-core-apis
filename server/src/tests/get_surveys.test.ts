import * as chai from 'chai';
import { Api, the, json } from './helpers';

Api.endpoint('/surveys').get(test => {

  json(test);

});
