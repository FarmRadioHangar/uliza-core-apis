import * as restify from 'restify';
import Server from './Server';
import { Voto } from './voto/Api';

import { BaseController }         from './controller/BaseController';
import { VotoResponseController } from './controller/VotoResponseController';

const api: Server = new Server('cert.pem', 'key.pem');
const voto: Voto.Api = new Voto.Api();

/* ••• Routes ••• */

const baseController = new BaseController();
const votoResponseController = new VotoResponseController();

api.get({path: '/api/v1/', version: '1.0.0'}, baseController.get);

/* Webhooks API */

api.post({path: '/api/v1/webhooks/voto/response', version: '1.0.0'}, votoResponseController.hook);

export default api;
