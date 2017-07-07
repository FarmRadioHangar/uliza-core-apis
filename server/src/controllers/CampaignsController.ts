import ApiController from './ApiController';
import Campaign      from '../models/campaign';

export default class CampaignsController extends ApiController {

  constructor() {
    super(Campaign);
  }

}
