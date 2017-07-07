import ApiController from './ApiController';
import Organization  from '../models/organization';

export default class OrganizationsController extends ApiController {

  constructor() {
    super(Organization);
  }

}
