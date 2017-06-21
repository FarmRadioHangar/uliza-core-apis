"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const restify = require("restify");
let api = restify.createServer({
    certificate: fs.readFileSync('cert.pem'),
    key: fs.readFileSync('key.pem'),
    name: 'Farm Radio API Server'
});
api.pre(restify.pre.sanitizePath());
function getBase(req, res, next) {
    res.json(200, {
        message: 'api.farmradio.fm'
    });
    return next();
}
api.get({ path: '/v1/', version: '1.0.0' }, getBase);
exports.default = api;
