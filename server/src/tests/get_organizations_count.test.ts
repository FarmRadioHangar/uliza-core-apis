import { Api, the, json } from './helpers';

Api.endpoint('/organizations/count').get(test => {

  json(test);

  the(test, 'should return a count of 21', response => {
    response.body.should.have.property('count');
    response.body.count.should.be.a('number');
    response.body.count.should.equal(21);
  });

});
