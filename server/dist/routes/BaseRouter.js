"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
class BaseRouter {
    /**
     * Initialize the HeroRouter
     */
    constructor() {
        this.router = express_1.Router();
        this.init();
    }
    getBase(req, res, next) {
        res.json({
            message: 'muliza api'
        });
    }
    init() {
        this.router.get('/', this.getBase);
    }
}
exports.BaseRouter = BaseRouter;
const baseRoutes = new BaseRouter();
baseRoutes.init();
exports.default = baseRoutes.router;
