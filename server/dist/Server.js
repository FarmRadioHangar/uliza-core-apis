"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const restify = require("restify");
const bunyan = require("bunyan");
const BaseController_1 = require("./controllers/BaseController");
const VotoResponseController_1 = require("./controllers/VotoResponseController");
let logger = bunyan.createLogger({
    name: 'audit',
    streams: [{
            path: 'access.log'
        }]
});
let api = restify.createServer({
    //  certificate: fs.readFileSync('cert.pem'),
    //  key: fs.readFileSync('key.pem'),
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
