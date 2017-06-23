import * as mocha from 'mocha';
import * as chai from 'chai';
import * as fs from 'fs';
import app from '../src/app';
import chaiHttp = require('chai-http');

chai.use(chaiHttp);

const expect = chai.expect;

process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";

describe('Application base endpoint', () => {

  it('should return json', () => {
    return chai.request(app.restify().server).get('/v1').then(res => {
        expect(res.type).to.eql('application/json');
    });
  });

  it('response should have a message property', () => {
    return chai.request(app.restify().server).get('/v1').then(res => {
      expect(res.body).to.have.property('message');
    });
  });

});
