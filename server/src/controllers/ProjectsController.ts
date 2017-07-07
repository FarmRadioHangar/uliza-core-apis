import ApiController from './ApiController';
import Project       from '../models/project';

export default class ProjectsController extends ApiController {

  constructor() {
    super(Project);
  }

}
