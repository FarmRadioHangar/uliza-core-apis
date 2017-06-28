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
const Koa = require("koa");
const Router = require("koa-router");
const bodyparser = require("koa-bodyparser");
const logger = require("koa-logger");
const cors = require("kcors");
let app = new Koa();
let api = new Router();
api.get('/', (ctx, next) => __awaiter(this, void 0, void 0, function* () {
    yield next();
    ctx.body = '200 OK';
    ctx.status = 200;
}));
app.use(cors())
    .use(logger())
    .use(api.routes())
    .use(api.allowedMethods())
    .use(bodyparser());
const port = process.env.PORT || 3000;
const env = process.env.NODE_ENV || 'development';
console.log(env);
app.listen(port);
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi9zcmMvaW5kZXgudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7OztBQUFBLDJCQUFrQztBQUNsQyxxQ0FBeUM7QUFDekMsNkNBQTZDO0FBQzdDLHFDQUF5QztBQUN6Qyw4QkFBb0M7QUFFcEMsSUFBSSxHQUFHLEdBQVEsSUFBSSxHQUFHLEVBQUUsQ0FBQztBQUN6QixJQUFJLEdBQUcsR0FBVyxJQUFJLE1BQU0sRUFBRSxDQUFDO0FBRS9CLEdBQUcsQ0FBQyxHQUFHLENBQUMsR0FBRyxFQUFFLENBQU8sR0FBRyxFQUFFLElBQUk7SUFDM0IsTUFBTSxJQUFJLEVBQUUsQ0FBQztJQUNiLEdBQUcsQ0FBQyxJQUFJLEdBQUcsUUFBUSxDQUFDO0lBQ3BCLEdBQUcsQ0FBQyxNQUFNLEdBQUcsR0FBRyxDQUFDO0FBQ25CLENBQUMsQ0FBQSxDQUFDLENBQUM7QUFFSCxHQUFHLENBQUMsR0FBRyxDQUFDLElBQUksRUFBRSxDQUFDO0tBQ1gsR0FBRyxDQUFDLE1BQU0sRUFBRSxDQUFDO0tBQ2IsR0FBRyxDQUFDLEdBQUcsQ0FBQyxNQUFNLEVBQUUsQ0FBQztLQUNqQixHQUFHLENBQUMsR0FBRyxDQUFDLGNBQWMsRUFBRSxDQUFDO0tBQ3pCLEdBQUcsQ0FBQyxVQUFVLEVBQUUsQ0FBQyxDQUFDO0FBRXRCLE1BQU0sSUFBSSxHQUFrQixPQUFPLENBQUMsR0FBRyxDQUFDLElBQUksSUFBSSxJQUFJLENBQUM7QUFDckQsTUFBTSxHQUFHLEdBQVcsT0FBTyxDQUFDLEdBQUcsQ0FBQyxRQUFRLElBQUksYUFBYSxDQUFDO0FBRTFELE9BQU8sQ0FBQyxHQUFHLENBQUMsR0FBRyxDQUFDLENBQUM7QUFFakIsR0FBRyxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCAqIGFzIEtvYSAgICAgICAgZnJvbSAna29hJztcbmltcG9ydCAqIGFzIFJvdXRlciAgICAgZnJvbSAna29hLXJvdXRlcic7XG5pbXBvcnQgKiBhcyBib2R5cGFyc2VyIGZyb20gJ2tvYS1ib2R5cGFyc2VyJztcbmltcG9ydCAqIGFzIGxvZ2dlciAgICAgZnJvbSAna29hLWxvZ2dlcic7XG5pbXBvcnQgKiBhcyBjb3JzICAgICAgIGZyb20gJ2tjb3JzJztcblxubGV0IGFwcDogS29hID0gbmV3IEtvYSgpO1xubGV0IGFwaTogUm91dGVyID0gbmV3IFJvdXRlcigpO1xuXG5hcGkuZ2V0KCcvJywgYXN5bmMgKGN0eCwgbmV4dCkgPT4ge1xuICBhd2FpdCBuZXh0KCk7XG4gIGN0eC5ib2R5ID0gJzIwMCBPSyc7XG4gIGN0eC5zdGF0dXMgPSAyMDA7XG59KTtcblxuYXBwLnVzZShjb3JzKCkpXG4gICAudXNlKGxvZ2dlcigpKVxuICAgLnVzZShhcGkucm91dGVzKCkpXG4gICAudXNlKGFwaS5hbGxvd2VkTWV0aG9kcygpKVxuICAgLnVzZShib2R5cGFyc2VyKCkpO1xuXG5jb25zdCBwb3J0OiBudW1iZXJ8c3RyaW5nID0gcHJvY2Vzcy5lbnYuUE9SVCB8fCAzMDAwO1xuY29uc3QgZW52OiBzdHJpbmcgPSBwcm9jZXNzLmVudi5OT0RFX0VOViB8fCAnZGV2ZWxvcG1lbnQnO1xuXG5jb25zb2xlLmxvZyhlbnYpO1xuXG5hcHAubGlzdGVuKHBvcnQpO1xuIl19