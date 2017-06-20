import { Router, Request, Response, NextFunction } from 'express';

export class BaseRouter {

  router: Router

  /**
   * Initialize the BaseRouterb
   */
  constructor() {
    this.router = Router();
    this.init();
  }

  public getBase(req: Request, res: Response, next: NextFunction) {
    res.json({
      message: 'muliza api'
    });
  }

  init() {
    this.router.get('/', this.getBase);
  }

}

let baseRoutes = new BaseRouter();
baseRoutes.init();

export default baseRoutes.router;
