import * as Router from 'koa-router';

import CampaignsController     from './controllers/CampaignsController';
import CountriesController     from './controllers/CountriesController';
import EpisodesController      from './controllers/EpisodesController';
import OrganizationsController from './controllers/OrganizationsController';
import ProgramsController      from './controllers/ProgramsController';
import ProjectsController      from './controllers/ProjectsController';
import SchedulesController     from './controllers/SchedulesController';
import SurveysController       from './controllers/SurveysController';
import TopicsController        from './controllers/TopicsController';

export default (api: Router) => {

  // -------------------------------------------------------------------------- 

  /**
   * TopicsController
   */
  const topicsController = new TopicsController();

  /**
   * List topics.
   */
  api.get('/topics', topicsController.collection);

  /**
   * Count the number of topics matching certain criteria.
   */
  api.get('/topics/count', topicsController.count);

  /**
   * Get detailed information about a topic.
   */
  api.get('/topics/:id', topicsController.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * SurveysController
   */
  const surveysController = new SurveysController();

  /**
   * List surveys.
   */
  api.get('/surveys', surveysController.collection);

  /**
   * Count the number of surveys matching certain criteria.
   */
  api.get('/surveys/count', surveysController.count);

  /**
   * Get detailed information about a survey.
   */
  api.get('/surveys/:id', surveysController.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * SchedulesController
   */
  const schedulesController = new SchedulesController();

  /**
   * List schedules.
   */
  api.get('/schedules', schedulesController.collection);

  /**
   * Count the number of schedules matching certain criteria.
   */
  api.get('/schedules/count', schedulesController.count);

  /**
   * Get detailed information about a schedule.
   */
  api.get('/schedules/:id', schedulesController.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * ProjectsController
   */
  const projectsController = new ProjectsController();

  /**
   * List projects.
   */
  api.get('/projects', projectsController.collection);

  /**
   * Count the number of projects matching certain criteria.
   */
  api.get('/projects/count', projectsController.count);

  /**
   * Get detailed information about a project.
   */
  api.get('/projects/:id', projectsController.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * ProgramsController
   */
  const programsController = new ProgramsController();

  /**
   * List programs.
   */
  api.get('/programs', programsController.collection);

  /**
   * Count the number of programs matching certain criteria.
   */
  api.get('/programs/count', programsController.count);

  /**
   * Get detailed information about a program.
   */
  api.get('/programs/:id', programsController.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * EpisodesController
   */
  const episodesController = new EpisodesController();

  /**
   * List episodes.
   */
  api.get('/episodes', episodesController.collection);

  /**
   * Count the number of episodes matching certain criteria.
   */
  api.get('/episodes/count', episodesController.count);

  /**
   * Get detailed information about an episode.
   */
  api.get('/episodes/:id', episodesController.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * CampaignsController
   */
  const campaignsController = new CampaignsController();

  /**
   * List campaigns.
   */
  api.get('/campaigns', campaignsController.collection);

  /**
   * Count the number of campaigns matching certain criteria.
   */
  api.get('/campaigns/count', campaignsController.count);

  /**
   * Get detailed information about a campaign.
   */
  api.get('/campaigns/:id', campaignsController.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * CountriesController
   */
  const countriesController = new CountriesController();

  /**
   * List countries.
   */
  api.get('/countries', countriesController.collection);

  /**
   * Count the number of countries matching certain criteria.
   */
  api.get('/countries/count', countriesController.count);

  /**
   * Get detailed information about a country.
   */
  api.get('/countries/:id', countriesController.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * OrganizationsController
   */
  const organizationsController = new OrganizationsController();

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

  // -------------------------------------------------------------------------- 

  api.get('/protected', async ctx => {
    ctx.body = { message: 'This API is a teapot.' };
  });

}
