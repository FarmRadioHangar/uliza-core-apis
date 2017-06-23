"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const bunyan = require("bunyan");
const debugStream = require("debug-stream");
const format = require("bunyan-format");
const restify = require("restify");
const stream = require("stream");
const BaseController_1 = require("./controllers/BaseController");
const VotoResponseController_1 = require("./controllers/VotoResponseController");
const transformer = new stream.Transform({ objectMode: true });
transformer._transform = function (chunk, encoding, next) {
    this.push(chunk);
    next();
};
transformer.pipe(debugStream('farm-radio-api:server')());
let logger = bunyan.createLogger({
    name: 'audit',
    streams: [
        {
            level: 'debug',
            path: 'access.log'
        },
        {
            stream: format({ outputMode: 'short' }, transformer)
        }
    ]
});
// var Trans = require('stream').Transform;
// 
// import { BaseController } from './controllers/BaseController';
// import { VotoResponseController } from './controllers/VotoResponseController';
// 
// var debugStream = require('debug-stream')('farm-radio-api:server');
// 
// //const transformer = new stream.Transform({ objectMode: true });
// const transformer = new Trans({ objectMode: true });
// 
// transformer._transform = (chunk: any, encoding: string, next: Function): void => {
//   this.push(chunk); 
//   next();
// }
// 
// transformer.pipe(debugStream());
// 
// let logger: bunyan = bunyan.createLogger({
//   name: 'audit',
//   streams: [
//     { 
//       level: 'debug',
//       path: 'access.log' 
//     }, 
//     { 
//       level: 'debug',
//       stream: format({ outputMode: 'short' }, transformer)
//     }
//   ]
// });
let api = restify.createServer({
    //certificate: fs.readFileSync('cert.pem'),
    //key: fs.readFileSync('key.pem'),
    name: 'Farm Radio API Server',
    log: logger
});
api.pre(restify.pre.sanitizePath());
api.on('after', restify.auditLogger({ log: logger }));
api.use(restify.fullResponse());
api.use(restify.bodyParser());
const baseController = new BaseController_1.BaseController();
const votoResponseController = new VotoResponseController_1.VotoResponseController();
api.get({ path: '/v1/', version: '1.0.0' }, baseController.get);
/* Webhooks API */
api.post({ path: '/v1/webhooks/voto/response', version: '1.0.0' }, votoResponseController.hook);
exports.default = api;
