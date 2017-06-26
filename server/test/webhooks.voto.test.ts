import * as mocha from 'mocha';
import * as chai from 'chai';
import * as fs from 'fs';
import app from '../src/app';
import chaiHttp = require('chai-http');

chai.use(chaiHttp);

const expect = chai.expect;

process.env.NODE_TLS_REJECT_UNAUTHORIZED = '0';

describe('VOTO response webhook', () => {

  it('should respond with 200 OK', () => {
    return chai.request(app.restify().server).post('/api/v1/webhooks/voto/response').then(res => {
      expect(res.status).to.eql(200);
    });
  });

});
