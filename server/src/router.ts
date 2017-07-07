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
  const topics = new TopicsController();

  /**
   * List topics.
   */
  api.get('/topics', topics.collection);

  /**
   * Count the number of topics matching certain criteria.
   */
  api.get('/topics/count', topics.count);

  /**
   * Get detailed information about a topic.
   */
  api.get('/topics/:id', topics.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * SurveysController
   */
  const surveys = new SurveysController();

  /**
   * List surveys.
   */
  api.get('/surveys', surveys.collection);

  /**
   * Count the number of surveys matching certain criteria.
   */
  api.get('/surveys/count', surveys.count);

  /**
   * Get detailed information about a survey.
   */
  api.get('/surveys/:id', surveys.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * SchedulesController
   */
  const schedules = new SchedulesController();

  /**
   * List schedules.
   */
  api.get('/schedules', schedules.collection);

  /**
   * Count the number of schedules matching certain criteria.
   */
  api.get('/schedules/count', schedules.count);

  /**
   * Get detailed information about a schedule.
   */
  api.get('/schedules/:id', schedules.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * ProjectsController
   */
  const projects = new ProjectsController();

  /**
   * List projects.
   */
  api.get('/projects', projects.collection);

  /**
   * Count the number of projects matching certain criteria.
   */
  api.get('/projects/count', projects.count);

  /**
   * Get detailed information about a project.
   */
  api.get('/projects/:id', projects.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * ProgramsController
   */
  const programs = new ProgramsController();

  /**
   * List programs.
   */
  api.get('/programs', programs.collection);

  /**
   * Count the number of programs matching certain criteria.
   */
  api.get('/programs/count', programs.count);

  /**
   * Get detailed information about a program.
   */
  api.get('/programs/:id', programs.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * EpisodesController
   */
  const episodes = new EpisodesController();

  /**
   * List episodes.
   */
  api.get('/episodes', episodes.collection);

  /**
   * Count the number of episodes matching certain criteria.
   */
  api.get('/episodes/count', episodes.count);

  /**
   * Get detailed information about an episode.
   */
  api.get('/episodes/:id', episodes.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * CampaignsController
   */
  const campaigns = new CampaignsController();

  /**
   * List campaigns.
   */
  api.get('/campaigns', campaigns.collection);

  /**
   * Count the number of campaigns matching certain criteria.
   */
  api.get('/campaigns/count', campaigns.count);

  /**
   * Get detailed information about a campaign.
   */
  api.get('/campaigns/:id', campaigns.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * CountriesController
   */
  const countries = new CountriesController();

  /**
   * List countries.
   */
  api.get('/countries', countries.collection);

  /**
   * Count the number of countries matching certain criteria.
   */
  api.get('/countries/count', countries.count);

  /**
   * Get detailed information about a country.
   */
  api.get('/countries/:id', countries.findOne);

  // -------------------------------------------------------------------------- 

  /**
   * OrganizationsController
   */
  const organizations = new OrganizationsController();

  /**
   * List organizations.
   */
  api.get('/organizations', organizations.collection);

  /**
   * Count the number of organizations matching certain criteria.
   */
  api.get('/organizations/count', organizations.count);

  /**
   * Get detailed information about an organization.
   */
  api.get('/organizations/:id', organizations.findOne);

  // -------------------------------------------------------------------------- 

  api.get('/protected', async ctx => {
    ctx.body = { message: 'This API is a teapot.' };
  });

}
