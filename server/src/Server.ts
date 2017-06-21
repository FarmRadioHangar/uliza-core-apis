import * as fs from 'fs';
import * as restify from 'restify';
import { Server, Request, Response, Next } from 'restify';

let api: Server = restify.createServer({
  certificate: fs.readFileSync('cert.pem'),
  key: fs.readFileSync('key.pem'),
  name: 'Farm Radio API Server'
});

api.pre(restify.pre.sanitizePath());

function getBase(req: Request, res: Response, next: Next) {
  res.json(200, {
    message: 'api.farmradio.fm'
  });
  return next();
}

function postVotoResponse(req: Request, res: Response, next: Next) {
  res.json(200);
  return next();
}

api.get({path: '/v1/', version: '1.0.0'}, getBase);

/*
 *  Webhooks API
 */

api.post({path: '/v1/webhooks/voto/response', version: '1.0.0'}, postVotoResponse);

export default api;
