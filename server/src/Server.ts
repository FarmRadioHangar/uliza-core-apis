import * as fs from 'fs';
import * as restify from 'restify';
import * as bunyan from 'bunyan';
import { BaseController } from './controllers/BaseController';
import { VotoResponseController } from './controllers/VotoResponseController';

let logger: bunyan = bunyan.createLogger({
  name: 'audit',
  streams: [{
    path: 'access.log'
  }]
});

let api: restify.Server = restify.createServer({
  //  certificate: fs.readFileSync('cert.pem'),
  //  key: fs.readFileSync('key.pem'),
  name: 'Farm Radio API Server',
  log: logger
});

api.pre(restify.pre.sanitizePath());

api.on('after', restify.auditLogger({ log: logger }));

api.use(restify.fullResponse());
api.use(restify.bodyParser());

const baseController = new BaseController();
const votoResponseController = new VotoResponseController();

api.get({path: '/v1/', version: '1.0.0'}, baseController.get);

/* Webhooks API */

api.post({path: '/v1/webhooks/voto/response', version: '1.0.0'}, votoResponseController.hook);

export default api;
