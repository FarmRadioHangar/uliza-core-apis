import ApiController from './ApiController';
import Country       from '../models/country';

export default class CountriesController extends ApiController {

  constructor() {
    super(Country);
  }

}
