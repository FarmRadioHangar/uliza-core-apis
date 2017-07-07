import { Api, the, json } from './helpers';

Api.endpoint('/organizations/count').get(test => {

  json(test);

  the(test, 'should return a count of 21', response => {
    response.body
      .should.have.a.property('count')
      .that.is.a('number')
      .and.equals(21);
  });

});

Api.endpoint('/organizations/count?id:gt=15').get(test => {

  json(test);

  the(test, 'should return a count of 6', response => {
    response.body
      .should.have.a.property('count')
      .that.is.a('number')
      .and.equals(6);
  });

});

Api.endpoint('/organizations/count?id=3').get(test => {

  json(test);

  the(test, 'should return a count of 1', response => {
    response.body
      .should.have.a.property('count')
      .that.is.a('number')
      .and.equals(1);
  });

});

Api.endpoint('/organizations/count?id:ne=3').get(test => {

  json(test);

  the(test, 'should return a count of 20', response => {
    response.body
      .should.have.a.property('count')
      .that.is.a('number')
      .and.equals(20);
  });

});
