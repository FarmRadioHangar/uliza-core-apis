import ApiController from './ApiController';
import Topic        from '../models/topic';

export default class TopicsController extends ApiController {

  constructor() {
    super(Topic);
  }

}
