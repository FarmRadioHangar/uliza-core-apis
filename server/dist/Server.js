"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const bunyan = require("bunyan");
const debug = require("debug");
const debugStream = require("debug-stream");
const format = require("bunyan-format");
const restify = require("restify");
class Server {
    constructor(certificate, key) {
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
    restify() {
        return this.api;
    }
    listen(...args) {
        this.api.listen(...args);
    }
    createLogger() {
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
    createRestifyServer() {
        this.api = restify.createServer({
            //certificate: fs.readFileSync(this.certificate),
            //key: fs.readFileSync(this.key),
            name: 'Farm Radio API Server',
            log: this.logger
        });
    }
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
    onListening() {
        const addr = this.api.address();
        const bind = typeof addr === 'string' ? `pipe ${addr}` : `port ${addr.port}`;
        this.debug(`Listening on ${bind}`);
    }
}
exports.default = Server;
