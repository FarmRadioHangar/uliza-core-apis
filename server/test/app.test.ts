import * as mocha from 'mocha';
import * as chai from 'chai';
import chaiHttp = require('chai-http');

import app from '../src/App';

chai.use(chaiHttp);

const expect = chai.expect;

describe('Application base endpoint', () => {

  it('should return json', () => {
    return chai.request(app).get('/').then(res => {
      expect(res.type).to.eql('application/json');
    });
  });

  it('response should have a message prop', () => {
    return chai.request(app).get('/').then(res => {
      expect(res.body.message).to.eql('muliza api');
    });
  });

});
