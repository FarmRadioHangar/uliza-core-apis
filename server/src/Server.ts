import * as bunyan      from 'bunyan';
import * as debug from 'debug';
import * as debugStream from 'debug-stream';
import * as format      from 'bunyan-format';
import * as fs          from 'fs';
import * as restify     from 'restify';
import * as stream      from 'stream';

const log = debug('farm-radio-api:server');

export default class Server {

  private api: restify.Server;

  private logger: bunyan;

  private transformer: stream.Transform;

  private certificate: string;

  private key: string;

  constructor(certificate: string, key: string) {
    this.certificate = certificate;
    this.key = key;
    this.createLogger();
    this.createRestifyServer();
    this.api.pre(restify.pre.sanitizePath());
    this.api.on('after', restify.auditLogger({ log: this.logger }));
    this.api.use(restify.fullResponse());
    this.api.use(restify.bodyParser());
    this.api.on('error', this.onError.bind(this));
    this.api.on('listening', this.onListening.bind(this));
  }

  public restify(): restify.Server {
    return this.api;
  }

  public listen(...args: any[]): void {
    this.api.listen(...args);
  }

  private createLogger(): void {
    this.transformer = new stream.Transform({ objectMode: true });
    this.transformer._transform = (chunk: any, encoding: string, next: () => void): void => {
      this.transformer.push(chunk); 
      next();
    }; 
    this.transformer.pipe(debugStream(log)());
    this.logger = bunyan.createLogger({
      name: 'access',
      streams: [
        { 
          level: 'debug',
          path: 'access.log' 
        }, 
        { 
          stream: format({ outputMode: 'short' }, this.transformer)
        }
      ]
    });
  }

  private createRestifyServer(): void {
    this.api = restify.createServer({
      //certificate: fs.readFileSync(this.certificate),
      //key: fs.readFileSync(this.key),
      name: 'Farm Radio API Server',
      log: this.logger
    });
  }

  private onError(error: NodeJS.ErrnoException): void {
    if (error.syscall !== 'listen') 
      throw error;
    switch(error.code) {
      case 'EACCES':
        console.error('Operation requires elevated privileges');
        process.exit(1);
        break;
      case 'EADDRINUSE':
        console.error('Address already in use');
        process.exit(1);
        break;
      default:
        throw error;
    }
  }

  private onListening(): void {
    const addr = this.api.address();
    const bind: string = typeof addr === 'string' ? `pipe ${addr}` : `port ${addr.port}`;
    log(`Listening on ${bind}`);
  }

}
