import * as bunyan      from 'bunyan';
import * as debug       from 'debug';
import * as debugStream from 'debug-stream';
import * as format      from 'bunyan-format';
import * as fs          from 'fs';
import * as restify     from 'restify';
import * as config      from 'config';

/**
 * ## API Server 
 *
 *
 * ### Typical use
 *
 * ```
 * let server: Server = new Server('cert.pem', 'key.pem');
 * server.listen(3000);
 * ```
 */
export default class Server {

  /**
   * @see http://restify.com/#server-api
   */
  private api: restify.Server;

  /**
   * @see https://github.com/visionmedia/debug#debug 
   */
  private debug: debug.IDebugger;

  /**
   * @see https://github.com/trentm/node-bunyan
   */
  private logger: bunyan;

  /**
   * Path to a PEM-encoded certificate.
   */
  private certificate: string;

  /**
   * Path to a PEM-encoded key.
   */
  private key: string;

  /**
   * Server name.
   */
  private name: string;

  /**
   * Create and initialize the API server.
   *
   * @param certificate Path to a PEM-encoded certificate.
   * @param key Path to a PEM-encoded key.
   */
  constructor(certificate: string, key: string) {
    this.certificate = certificate;
    this.key = key;
    this.readConfig();
    this.createLogger();
    this.createRestifyServer();
    this.api.pre(restify.pre.sanitizePath());
    this.api.on('after', restify.auditLogger({ log: this.logger }));
    this.api.use(restify.fullResponse());
    this.api.use(restify.bodyParser());
    this.api.on('error', this.onError.bind(this));
    this.api.on('listening', this.onListening.bind(this));
  }

  /**
   * Return the restify server. Use this method to directly access the restify 
   * server API.
   *
   * @see http://restify.com/#server-api
   *
   * @returns A restify server instance.
   */
  public restify(): restify.Server {
    return this.api;
  }

  /**
   * Begin accepting connections.
   * 
   * @see https://nodejs.org/docs/latest/api/http.html#http_server_listen_port_hostname_backlog_callback
   */
  public listen(...args: any[]): void {
    this.api.listen(...args);
  }

  public get(route: any, routeCallBack: restify.RequestHandler | restify.RequestHandler[], ...routeCallBacks: restify.RequestHandler[][]): string {
    return this.api.get(route, routeCallBack, ...routeCallBacks);
  }

  public post(route: any, routeCallBack: restify.RequestHandler | restify.RequestHandler[], ...routeCallBacks: restify.RequestHandler[][]): string {
    return this.api.post(route, routeCallBack, ...routeCallBacks);
  }

  public put(route: any, routeCallBack: restify.RequestHandler | restify.RequestHandler[], ...routeCallBacks: restify.RequestHandler[][]): string {
    return this.api.put(route, routeCallBack, ...routeCallBacks);
  }

  /**
   * @private
   */
  private createLogger(): void {
    this.debug = debug('farm-radio-api:server');
    this.logger = bunyan.createLogger({
      name: 'access',
      streams: [
        { 
          level: 'debug',
          path: 'access.log' 
        }, 
        { 
          stream: format({ outputMode: 'short' }, debugStream(this.debug)())
        }
      ]
    });
  }

  /**
   * @private
   */
  private createRestifyServer(): void {
    this.api = restify.createServer({
      //certificate: fs.readFileSync(this.certificate),
      //key: fs.readFileSync(this.key),
      name: this.name,
      log: this.logger
    });
  }

  /**
   * @private
   */
  private readConfig(): void {
    this.name = config.has('server.name') ? config.get<string>('server.name') : 'Farm Radio API Server';
  }

  /**
   * @private
   */
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

  /**
   * @private
   */
  private onListening(): void {
    const addr = this.api.address();
    const bind: string = typeof addr === 'string' ? `pipe ${addr}` : `port ${addr.port}`;
    this.debug(`Listening on ${bind}`);
  }

}
