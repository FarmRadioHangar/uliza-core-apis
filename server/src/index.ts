import * as debug from 'debug';
import * as fs from 'fs';
import * as restify from 'restify';

debug('farm-radio-apis:server');

const server = restify.createServer({
  certificate: fs.readFileSync('cert.pem'),
  key: fs.readFileSync('key.pem'),
  name: 'Farm Radio API Server',
});

const port = normalizePort(process.env.PORT || 3000);

server.pre(restify.pre.sanitizePath());

server.on('error', onError);
server.on('listening', onListening);

function getBase(req: restify.Request, res: restify.Response, next: restify.Next) {
  res.json(200, 'api.farmradio.fm');
  return next();
}

server.get({path: '/v1/', version: '1.0.0'}, getBase);

server.listen(port);

function normalizePort(val: number|string): number|string|boolean {
  let port: number = (typeof val === 'string') ? parseInt(val, 10) 
                                               : val;
  if (isNaN(port)) 
    return val;
  else if (port >= 0) 
    return port;
  else 
    return false;
}

function onListening(): void {
  let addr = server.address();
  let bind: string = (typeof addr === 'string') ? `pipe ${addr}` 
                                                : `port ${addr.port}`;
  debug(`Server listening on ${bind}`);
}

function onError(error: NodeJS.ErrnoException): void {
  if (error.syscall !== 'listen') 
    throw error;
  let bind = (typeof port === 'string') ? 'Pipe ' + port : 'Port ' + port;
  switch(error.code) {
    case 'EACCES':
      console.error(`${bind} requires elevated privileges`);
      process.exit(1);
      break;
    case 'EADDRINUSE':
      console.error(`${bind} is already in use`);
      process.exit(1);
      break;
    default:
      throw error;
  }
}
