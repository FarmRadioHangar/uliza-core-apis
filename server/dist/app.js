"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const Server_1 = require("./Server");
const Api_1 = require("./voto/Api");
const BaseController_1 = require("./controllers/BaseController");
const VotoResponseController_1 = require("./controllers/VotoResponseController");
const api = new Server_1.default('cert.pem', 'key.pem');
const voto = new Api_1.Voto.Api();
/* ••• Routes ••• */
const baseController = new BaseController_1.BaseController();
const votoResponseController = new VotoResponseController_1.VotoResponseController();
api.get({ path: '/v1/', version: '1.0.0' }, baseController.get);
/* Webhooks API */
api.post({ path: '/v1/webhooks/voto/response', version: '1.0.0' }, votoResponseController.hook);
exports.default = api;
