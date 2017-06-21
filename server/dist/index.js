"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const debug = require("debug");
const Server_1 = require("./Server");
debug('farm-radio-apis:server');
const port = normalizePort(process.env.PORT || 3000);
Server_1.default.on('error', onError);
Server_1.default.on('listening', onListening);
Server_1.default.listen(port);
function normalizePort(val) {
    let port = (typeof val === 'string') ? parseInt(val, 10)
        : val;
    if (isNaN(port))
        return val;
    else if (port >= 0)
        return port;
    else
        return false;
}
function onListening() {
    let addr = Server_1.default.address();
    let bind = (typeof addr === 'string') ? `pipe ${addr}`
        : `port ${addr.port}`;
    debug(`Server listening on ${bind}`);
}
function onError(error) {
    if (error.syscall !== 'listen')
        throw error;
    let bind = (typeof port === 'string') ? 'Pipe ' + port : 'Port ' + port;
    switch (error.code) {
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
