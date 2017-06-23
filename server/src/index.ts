require('dotenv').config();

import Server from './Server';

import { BaseController }         from './controllers/BaseController';
import { VotoResponseController } from './controllers/VotoResponseController';

const server: Server = new Server('cert.pem', 'key.pem');

server.listen(normalized(process.env.PORT || 3000));

function normalized(val: number|string): number|string {
  const port: number = typeof val === 'string' ? parseInt(val, 10) : val;
  if (isNaN(port)) 
    return val;
  else if (port >= 0) 
    return port;
  console.error('Bad port');
  process.exit(1);
}

/* ••• Routes ••• */

const api = server.restify();

const baseController = new BaseController();
const votoResponseController = new VotoResponseController();

api.get({path: '/v1/', version: '1.0.0'}, baseController.get);

/* Webhooks API */

api.post({path: '/v1/webhooks/voto/response', version: '1.0.0'}, votoResponseController.hook);
