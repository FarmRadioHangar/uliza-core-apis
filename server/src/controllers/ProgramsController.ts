import ApiController from './ApiController';
import Program       from '../models/program';

export default class ProgramsController extends ApiController {

  constructor() {
    super(Program);
  }

}
