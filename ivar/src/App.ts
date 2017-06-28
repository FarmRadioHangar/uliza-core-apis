import * as Koa from "koa";
import * as Router from "koa-router";
import * as bodyParser from "koa-bodyparser";
import * as logger from "koa-logger";
import { Server } from 'http';

export default class App {

  constructor() {
  }

  private async init(): Promise<Koa> {
    const app: Koa = new Koa();
    const router: Router = new Router();

    app.use(logger())
       .use(bodyParser())
       .use(router.routes())
       .use(router.allowedMethods());

    return Promise.resolve(app);
  }

  public async start(): Promise<Server> {
    const app: Koa = await this.init();
    console.log('Listening on port 3000');
    return Promise.resolve(app.listen(3000));
  }

}
