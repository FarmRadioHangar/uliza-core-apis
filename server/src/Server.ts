import * as bunyan      from 'bunyan';
import * as debug       from 'debug';
import * as debugStream from 'debug-stream';
import * as format      from 'bunyan-format';
import * as fs          from 'fs';
import * as restify     from 'restify';
import * as stream      from 'stream';

import { BaseController } from './controllers/BaseController';
import { VotoResponseController } from './controllers/VotoResponseController';

const transformer: stream.Transform = new stream.Transform({ 
  objectMode: true 
});

transformer._transform = function(chunk: any, encoding: string, next: Function): void {
  this.push(chunk); 
  next();
}

transformer.pipe(debugStream('farm-radio-api:server')());

let logger: bunyan = bunyan.createLogger({
  name: 'audit',
  streams: [
    { 
      level: 'debug',
      path: 'access.log' 
    }, 
    { 
      stream: format({ outputMode: 'short' }, transformer)
    }
  ]
});

let api: restify.Server = restify.createServer({
  //certificate: fs.readFileSync('cert.pem'),
  //key: fs.readFileSync('key.pem'),
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
