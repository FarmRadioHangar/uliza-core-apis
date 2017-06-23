"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
require('dotenv').config();
const Server_1 = require("./Server");
const BaseController_1 = require("./controllers/BaseController");
const VotoResponseController_1 = require("./controllers/VotoResponseController");
const port = normalizePort(process.env.PORT || 3000);
const server = new Server_1.default('cert.pem', 'key.pem');
server.listen(port);
function normalizePort(val) {
    const port = typeof val === 'string' ? parseInt(val, 10) : val;
    if (isNaN(port))
        return val;
    else if (port >= 0)
        return port;
    console.error('Bad port');
    process.exit(1);
}
/* Routes */
const api = server.restify();
const baseController = new BaseController_1.BaseController();
const votoResponseController = new VotoResponseController_1.VotoResponseController();
api.get({ path: '/v1/', version: '1.0.0' }, baseController.get);
/* Webhooks API */
api.post({ path: '/v1/webhooks/voto/response', version: '1.0.0' }, votoResponseController.hook);
