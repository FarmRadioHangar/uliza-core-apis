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
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiQXBwLmpzIiwic291cmNlUm9vdCI6Ii9ob21lL2pvaGFubmVzL3dvcmsvdWxpemEtY29yZS1hcGlzL2l2YXIvc3JjLyIsInNvdXJjZXMiOlsiQXBwLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7Ozs7Ozs7Ozs7QUFBQSwyQkFBMkI7QUFDM0IscUNBQXFDO0FBQ3JDLDZDQUE2QztBQUM3QyxxQ0FBcUM7QUFHckM7SUFFRTtJQUNBLENBQUM7SUFFYSxJQUFJOztZQUNoQixNQUFNLEdBQUcsR0FBUSxJQUFJLEdBQUcsRUFBRSxDQUFDO1lBQzNCLE1BQU0sTUFBTSxHQUFXLElBQUksTUFBTSxFQUFFLENBQUM7WUFFcEMsR0FBRyxDQUFDLEdBQUcsQ0FBQyxNQUFNLEVBQUUsQ0FBQztpQkFDYixHQUFHLENBQUMsVUFBVSxFQUFFLENBQUM7aUJBQ2pCLEdBQUcsQ0FBQyxNQUFNLENBQUMsTUFBTSxFQUFFLENBQUM7aUJBQ3BCLEdBQUcsQ0FBQyxNQUFNLENBQUMsY0FBYyxFQUFFLENBQUMsQ0FBQztZQUVqQyxNQUFNLENBQUMsT0FBTyxDQUFDLE9BQU8sQ0FBQyxHQUFHLENBQUMsQ0FBQztRQUM5QixDQUFDO0tBQUE7SUFFWSxLQUFLOztZQUNoQixNQUFNLEdBQUcsR0FBUSxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQztZQUNuQyxPQUFPLENBQUMsR0FBRyxDQUFDLHdCQUF3QixDQUFDLENBQUM7WUFDdEMsTUFBTSxDQUFDLE9BQU8sQ0FBQyxPQUFPLENBQUMsR0FBRyxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDO1FBQzNDLENBQUM7S0FBQTtDQUVGO0FBdkJELHNCQXVCQyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCAqIGFzIEtvYSBmcm9tIFwia29hXCI7XG5pbXBvcnQgKiBhcyBSb3V0ZXIgZnJvbSBcImtvYS1yb3V0ZXJcIjtcbmltcG9ydCAqIGFzIGJvZHlQYXJzZXIgZnJvbSBcImtvYS1ib2R5cGFyc2VyXCI7XG5pbXBvcnQgKiBhcyBsb2dnZXIgZnJvbSBcImtvYS1sb2dnZXJcIjtcbmltcG9ydCB7IFNlcnZlciB9IGZyb20gJ2h0dHAnO1xuXG5leHBvcnQgZGVmYXVsdCBjbGFzcyBBcHAge1xuXG4gIGNvbnN0cnVjdG9yKCkge1xuICB9XG5cbiAgcHJpdmF0ZSBhc3luYyBpbml0KCk6IFByb21pc2U8S29hPiB7XG4gICAgY29uc3QgYXBwOiBLb2EgPSBuZXcgS29hKCk7XG4gICAgY29uc3Qgcm91dGVyOiBSb3V0ZXIgPSBuZXcgUm91dGVyKCk7XG5cbiAgICBhcHAudXNlKGxvZ2dlcigpKVxuICAgICAgIC51c2UoYm9keVBhcnNlcigpKVxuICAgICAgIC51c2Uocm91dGVyLnJvdXRlcygpKVxuICAgICAgIC51c2Uocm91dGVyLmFsbG93ZWRNZXRob2RzKCkpO1xuXG4gICAgcmV0dXJuIFByb21pc2UucmVzb2x2ZShhcHApO1xuICB9XG5cbiAgcHVibGljIGFzeW5jIHN0YXJ0KCk6IFByb21pc2U8U2VydmVyPiB7XG4gICAgY29uc3QgYXBwOiBLb2EgPSBhd2FpdCB0aGlzLmluaXQoKTtcbiAgICBjb25zb2xlLmxvZygnTGlzdGVuaW5nIG9uIHBvcnQgMzAwMCcpO1xuICAgIHJldHVybiBQcm9taXNlLnJlc29sdmUoYXBwLmxpc3RlbigzMDAwKSk7XG4gIH1cblxufVxuIl19