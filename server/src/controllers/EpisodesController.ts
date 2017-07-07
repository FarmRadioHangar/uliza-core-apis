import ApiController from './ApiController';
import Episode       from '../models/episode';

export default class EpisodesController extends ApiController {

  constructor() {
    super(Episode);
  }

}
