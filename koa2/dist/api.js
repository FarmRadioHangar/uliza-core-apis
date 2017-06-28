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
const Router = require("koa-router");
var Api;
(function (Api) {
    let api = new Router();
    api.get('/', (ctx, next) => __awaiter(this, void 0, void 0, function* () {
        yield next();
        ctx.body = 'body';
        ctx.status = 200;
    }));
    Api.router = api;
})(Api = exports.Api || (exports.Api = {}));
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYXBpLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vc3JjL2FwaS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7Ozs7Ozs7O0FBQUEscUNBQXFDO0FBRXJDLElBQWMsR0FBRyxDQVloQjtBQVpELFdBQWMsR0FBRztJQUVmLElBQUksR0FBRyxHQUFXLElBQUksTUFBTSxFQUFFLENBQUM7SUFFL0IsR0FBRyxDQUFDLEdBQUcsQ0FBQyxHQUFHLEVBQUUsQ0FBTyxHQUFHLEVBQUUsSUFBSTtRQUMzQixNQUFNLElBQUksRUFBRSxDQUFDO1FBQ2IsR0FBRyxDQUFDLElBQUksR0FBRyxNQUFNLENBQUM7UUFDbEIsR0FBRyxDQUFDLE1BQU0sR0FBRyxHQUFHLENBQUM7SUFDbkIsQ0FBQyxDQUFBLENBQUMsQ0FBQztJQUVVLFVBQU0sR0FBRyxHQUFHLENBQUM7QUFFNUIsQ0FBQyxFQVphLEdBQUcsR0FBSCxXQUFHLEtBQUgsV0FBRyxRQVloQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCAqIGFzIFJvdXRlciBmcm9tICdrb2Etcm91dGVyJztcblxuZXhwb3J0IG1vZHVsZSBBcGkge1xuXG4gIGxldCBhcGk6IFJvdXRlciA9IG5ldyBSb3V0ZXIoKTtcblxuICBhcGkuZ2V0KCcvJywgYXN5bmMgKGN0eCwgbmV4dCkgPT4ge1xuICAgIGF3YWl0IG5leHQoKTtcbiAgICBjdHguYm9keSA9ICdib2R5JztcbiAgICBjdHguc3RhdHVzID0gMjAwO1xuICB9KTtcblxuICBleHBvcnQgY29uc3Qgcm91dGVyID0gYXBpO1xuXG59XG4iXX0=