"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
require('dotenv').config();
const debug = require("debug");
const Server_1 = require("./Server");
const log = debug('farm-radio-api:server'), port = normalizePort(process.env.PORT || 3000);
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
    const addr = Server_1.default.address();
    const bind = (typeof addr === 'string') ? `pipe ${addr}`
        : `port ${addr.port}`;
    log(`Listening on ${bind}`);
}
function onError(error) {
    if (error.syscall !== 'listen')
        throw error;
    const bind = (typeof port === 'string') ? 'Pipe ' + port : 'Port ' + port;
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
