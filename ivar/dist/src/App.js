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
const bodyParser = require("koa-bodyparser");
const logger = require("koa-logger");
class App {
    constructor() {
    }
    init() {
        return __awaiter(this, void 0, void 0, function* () {
            const app = new Koa();
            const router = new Router();
            app.use(logger())
                .use(bodyParser())
                .use(router.routes())
                .use(router.allowedMethods());
            return Promise.resolve(app);
        });
    }
    start() {
        return __awaiter(this, void 0, void 0, function* () {
            const app = yield this.init();
            console.log('Listening on port 3000');
            return Promise.resolve(app.listen(3000));
        });
    }
}
exports.default = App;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiQXBwLmpzIiwic291cmNlUm9vdCI6Ii9ob21lL2pvaGFubmVzL3dvcmsvdWxpemEtY29yZS1hcGlzL2l2YXIvc3JjLyIsInNvdXJjZXMiOlsic3JjL0FwcC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7Ozs7Ozs7O0FBQUEsMkJBQTJCO0FBQzNCLHFDQUFxQztBQUNyQyw2Q0FBNkM7QUFDN0MscUNBQXFDO0FBR3JDO0lBRUU7SUFDQSxDQUFDO0lBRWEsSUFBSTs7WUFDaEIsTUFBTSxHQUFHLEdBQVEsSUFBSSxHQUFHLEVBQUUsQ0FBQztZQUMzQixNQUFNLE1BQU0sR0FBVyxJQUFJLE1BQU0sRUFBRSxDQUFDO1lBRXBDLEdBQUcsQ0FBQyxHQUFHLENBQUMsTUFBTSxFQUFFLENBQUM7aUJBQ2IsR0FBRyxDQUFDLFVBQVUsRUFBRSxDQUFDO2lCQUNqQixHQUFHLENBQUMsTUFBTSxDQUFDLE1BQU0sRUFBRSxDQUFDO2lCQUNwQixHQUFHLENBQUMsTUFBTSxDQUFDLGNBQWMsRUFBRSxDQUFDLENBQUM7WUFFakMsTUFBTSxDQUFDLE9BQU8sQ0FBQyxPQUFPLENBQUMsR0FBRyxDQUFDLENBQUM7UUFDOUIsQ0FBQztLQUFBO0lBRVksS0FBSzs7WUFDaEIsTUFBTSxHQUFHLEdBQVEsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDbkMsT0FBTyxDQUFDLEdBQUcsQ0FBQyx3QkFBd0IsQ0FBQyxDQUFDO1lBQ3RDLE1BQU0sQ0FBQyxPQUFPLENBQUMsT0FBTyxDQUFDLEdBQUcsQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQztRQUMzQyxDQUFDO0tBQUE7Q0FFRjtBQXZCRCxzQkF1QkMiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgKiBhcyBLb2EgZnJvbSBcImtvYVwiO1xuaW1wb3J0ICogYXMgUm91dGVyIGZyb20gXCJrb2Etcm91dGVyXCI7XG5pbXBvcnQgKiBhcyBib2R5UGFyc2VyIGZyb20gXCJrb2EtYm9keXBhcnNlclwiO1xuaW1wb3J0ICogYXMgbG9nZ2VyIGZyb20gXCJrb2EtbG9nZ2VyXCI7XG5pbXBvcnQgeyBTZXJ2ZXIgfSBmcm9tICdodHRwJztcblxuZXhwb3J0IGRlZmF1bHQgY2xhc3MgQXBwIHtcblxuICBjb25zdHJ1Y3RvcigpIHtcbiAgfVxuXG4gIHByaXZhdGUgYXN5bmMgaW5pdCgpOiBQcm9taXNlPEtvYT4ge1xuICAgIGNvbnN0IGFwcDogS29hID0gbmV3IEtvYSgpO1xuICAgIGNvbnN0IHJvdXRlcjogUm91dGVyID0gbmV3IFJvdXRlcigpO1xuXG4gICAgYXBwLnVzZShsb2dnZXIoKSlcbiAgICAgICAudXNlKGJvZHlQYXJzZXIoKSlcbiAgICAgICAudXNlKHJvdXRlci5yb3V0ZXMoKSlcbiAgICAgICAudXNlKHJvdXRlci5hbGxvd2VkTWV0aG9kcygpKTtcblxuICAgIHJldHVybiBQcm9taXNlLnJlc29sdmUoYXBwKTtcbiAgfVxuXG4gIHB1YmxpYyBhc3luYyBzdGFydCgpOiBQcm9taXNlPFNlcnZlcj4ge1xuICAgIGNvbnN0IGFwcDogS29hID0gYXdhaXQgdGhpcy5pbml0KCk7XG4gICAgY29uc29sZS5sb2coJ0xpc3RlbmluZyBvbiBwb3J0IDMwMDAnKTtcbiAgICByZXR1cm4gUHJvbWlzZS5yZXNvbHZlKGFwcC5saXN0ZW4oMzAwMCkpO1xuICB9XG5cbn1cbiJdfQ==