import * as fs from 'fs';
import * as restify from 'restify';

let api: restify.Server = restify.createServer({
  certificate: fs.readFileSync('cert.pem'),
  key: fs.readFileSync('key.pem'),
  name: 'Farm Radio API Server'
});

api.pre(restify.pre.sanitizePath());

function getBase(req: restify.Request, res: restify.Response, next: restify.Next) {
  res.json(200, {
    message: 'api.farmradio.fm'
  });
  return next();
}

api.get({path: '/v1/', version: '1.0.0'}, getBase);

export default api;
