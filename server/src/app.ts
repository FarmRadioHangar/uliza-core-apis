import * as restify from 'restify';
import Server from './Server';
import { Voto } from './voto/Api';

import { BaseController }         from './controllers/BaseController';
import { VotoResponseController } from './controllers/VotoResponseController';

const api: Server = new Server('cert.pem', 'key.pem');
const voto: Voto.Api = new Voto.Api();

console.log(voto.buildUrl('trees', { limit: 10 }));

/* ••• Routes ••• */

const baseController = new BaseController();
const votoResponseController = new VotoResponseController();

api.get({path: '/v1/', version: '1.0.0'}, baseController.get);

/* Webhooks API */

api.post({path: '/v1/webhooks/voto/response', version: '1.0.0'}, votoResponseController.hook);

export default api;
