"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class BaseController {
    constructor() { }
    get(req, res, next) {
        res.json(200, {
            message: 'api.farmradio.fm'
        });
        return next();
    }
}
exports.BaseController = BaseController;
