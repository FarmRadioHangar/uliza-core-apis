"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
require("reflect-metadata");
const Koa = require("koa");
const bodyparser = require("koa-bodyparser");
const typeorm_1 = require("typeorm");
typeorm_1.createConnection().then((connection) => __awaiter(this, void 0, void 0, function* () {
    let app = new Koa();
    const port = 3030;
    app.use(bodyparser());
    app.listen(port);
    console.log(`Koa application is up and running on port ${port}`);
})).catch(error => {
    console.log(console.log('Connection error'));
});
//# sourceMappingURL=index.js.map