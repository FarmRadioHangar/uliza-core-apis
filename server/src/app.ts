import * as restify from 'restify';
import Server from './Server';

import { BaseController }         from './controllers/BaseController';
import { VotoResponseController } from './controllers/VotoResponseController';

const api: Server = new Server('cert.pem', 'key.pem');

/* ••• Routes ••• */

const baseController = new BaseController();
const votoResponseController = new VotoResponseController();

api.get({path: '/v1/', version: '1.0.0'}, baseController.get);

/* Webhooks API */

api.post({path: '/v1/webhooks/voto/response', version: '1.0.0'}, votoResponseController.hook);

export default api;
