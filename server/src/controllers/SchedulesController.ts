import ApiController from './ApiController';
import Schedule      from '../models/schedule';

export default class SchedulesController extends ApiController {

  constructor() {
    super(Schedule);
  }

}
