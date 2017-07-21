import { Api, the, assertJson200 } from './helpers';

Api.endpoint('/countries/count').get(test => {

  assertJson200(test);

  the(test, 'should return a count of 6', response => {
    response.body
      .should.have.a.property('count')
      .that.is.a('number')
      .and.equals(6);
  });

});
