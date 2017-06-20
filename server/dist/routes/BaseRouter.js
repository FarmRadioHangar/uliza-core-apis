"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
class BaseRouter {
    /**
     * Initialize the BaseRouter
     */
    constructor() {
        this.router = express_1.Router();
        this.init();
    }
    getBase(req, res, next) {
        res.json({
            message: 'api.uliza.fm'
        });
    }
    init() {
        this.router.get('/', this.getBase);
    }
}
exports.BaseRouter = BaseRouter;
exports.default = new BaseRouter().router;
