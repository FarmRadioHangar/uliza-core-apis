import * as debug from 'debug';
import server from './Server';

debug('farm-radio-apis:server');

const port = normalizePort(process.env.PORT || 3000);

server.on('error', onError);
server.on('listening', onListening);

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
  const addr = server.address();
  const bind: string = (typeof addr === 'string') ? `pipe ${addr}` 
                                                  : `port ${addr.port}`;
  debug(`Server listening on ${bind}`);
}

function onError(error: NodeJS.ErrnoException): void {
  if (error.syscall !== 'listen') 
    throw error;
  const bind = (typeof port === 'string') ? 'Pipe ' + port : 'Port ' + port;
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
