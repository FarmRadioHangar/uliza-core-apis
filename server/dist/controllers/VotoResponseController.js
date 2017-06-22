"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class VotoResponseController {
    constructor() { }
    hook(req, res, next) {
        const body = req.params;
        const questionId = Number(body.question_id);
        const surveyId = Number(body.survey_id);
        res.json(200);
        return next();
    }
}
exports.VotoResponseController = VotoResponseController;
