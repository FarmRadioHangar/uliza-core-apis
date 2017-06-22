"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const restify = require("restify");
const bunyan = require("bunyan");
let logger = bunyan.createLogger({
    name: 'audit',
    streams: [
        { path: 'access.log' }
    ]
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
function getBase(req, res, next) {
    res.json(200, {
        message: 'api.farmradio.fm'
    });
    return next();
}
function postVotoResponse(req, res, next) {
    const body = req.params;
    const questionId = Number(body.question_id);
    const surveyId = Number(body.survey_id);
    res.json(200);
    return next();
}
api.get({ path: '/v1/', version: '1.0.0' }, getBase);
/* Webhooks API */
api.post({ path: '/v1/webhooks/voto/response', version: '1.0.0' }, postVotoResponse);
exports.default = api;
