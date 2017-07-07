import * as Router from 'koa-router';

import CountriesController     from './controllers/CountriesController';
import OrganizationsController from './controllers/OrganizationsController';

export default (api: Router) => {

  /**
   * CountriesController
   */
  const countriesController = new CountriesController();

  /**
   * OrganizationsController
   */
  const organizationsController = new OrganizationsController();

  /**
   * List countries.
   */
  api.get('/countries', countriesController.collection);

  /**
   * List organizations.
   */
  api.get('/organizations', organizationsController.collection);

  /**
   * Count the number of organizations matching certain criteria.
   */
  api.get('/organizations/count', organizationsController.count);

  /**
   * Get detailed information about an organization.
   */
  api.get('/organizations/:id', organizationsController.findOne);

  api.get('/protected', async ctx => {
    ctx.body = { message: 'This API is a teapot.' };
  });

}
