import { Router, Request, Response, NextFunction } from 'express';

export class BaseRouter {

  router: Router;

  /**
   * Initialize the BaseRouter
   */
  constructor() {
    this.router = Router();
    this.init();
  }

  public getBase(req: Request, res: Response, next: NextFunction): void {
    res.json({
      message: 'api.farmradio.fm'
    });
  }

  private init(): void {
    this.router.get('/', this.getBase);
  }

}

export default new BaseRouter().router;
