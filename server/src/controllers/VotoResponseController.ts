import { Request, Response, Next } from 'restify';

export class VotoResponseController {

  constructor() {}

  public hook(req: Request, res: Response, next: Next): void {
    const body = req.params;
    const questionId: number = Number(body.question_id);
    const surveyId: number = Number(body.survey_id);
    res.json(200);
    return next();
  }

}
