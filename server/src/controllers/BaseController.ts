import { Request, Response, Next } from 'restify';

export class BaseController {

  constructor() {}

  public get(req: Request, res: Response, next: Next): void {
    res.json(200, {
      message: 'api.farmradio.fm'
    });
    return next();
  }

}
