"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const bunyan = require("bunyan");
const config = require("config");
const debug = require("debug");
const debugStream = require("debug-stream");
const format = require("bunyan-format");
const normalize = require("normalize-path");
const path = require("path");
const restify = require("restify");
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
class Server {
    /**
     * Create and initialize the API server.
     *
     * @param certificate Path to a PEM-encoded certificate.
     * @param key Path to a PEM-encoded key.
     */
    constructor(certificate, key) {
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
     * @see {@link http://restify.com/#server-api}
     *
     * @returns A restify server instance.
     */
    restify() {
        return this.api;
    }
    /**
     * Begin accepting connections.
     *
     * @see {@link https://nodejs.org/docs/latest/api/http.html#http_server_listen_port_hostname_backlog_callback}
     */
    listen(...args) {
        this.api.listen(...args);
    }
    get(route, routeCallBack, ...routeCallBacks) {
        return this.api.get(route, routeCallBack, ...routeCallBacks);
    }
    post(route, routeCallBack, ...routeCallBacks) {
        return this.api.post(route, routeCallBack, ...routeCallBacks);
    }
    put(route, routeCallBack, ...routeCallBacks) {
        return this.api.put(route, routeCallBack, ...routeCallBacks);
    }
    /**
     * @private
     */
    createLogger() {
        const logPath = config.has('logs.access.path')
            ? config.get('logs.access.path') : '';
        this.debug = debug('farm-radio-api:server');
        this.logger = bunyan.createLogger({
            name: 'access',
            streams: [
                {
                    level: 'debug',
                    path: `${normalize(path.normalize(logPath))}/access.log`
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
    createRestifyServer() {
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
    readConfig() {
        this.name = config.has('server.name') ? config.get('server.name') : 'Farm Radio API Server';
    }
    /**
     * @private
     */
    onError(error) {
        if (error.syscall !== 'listen')
            throw error;
        switch (error.code) {
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
    onListening() {
        const addr = this.api.address();
        const bind = typeof addr === 'string' ? `pipe ${addr}` : `port ${addr.port}`;
        this.debug(`Listening on ${bind}`);
    }
}
exports.default = Server;
