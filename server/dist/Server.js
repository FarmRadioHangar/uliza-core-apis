"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const restify = require("restify");
let api = restify.createServer({
    //  certificate: fs.readFileSync('cert.pem'),
    //  key: fs.readFileSync('key.pem'),
    name: 'Farm Radio API Server'
});
api.pre(restify.pre.sanitizePath());
api.use(restify.fullResponse());
api.use(restify.bodyParser());
function getBase(req, res, next) {
    res.json(200, {
        message: 'api.farmradio.fm'
    });
    return next();
}
function postVotoResponse(req, res, next) {
    console.log(req.body);
    res.json(200);
    return next();
}
api.get({ path: '/v1/', version: '1.0.0' }, getBase);
/* Webhooks API */
api.post({ path: '/v1/webhooks/voto/response', version: '1.0.0' }, postVotoResponse);
exports.default = api;
