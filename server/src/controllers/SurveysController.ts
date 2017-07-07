import ApiController from './ApiController';
import Survey        from '../models/survey';

export default class SurveysController extends ApiController {

  constructor() {
    super(Survey);
  }

}
