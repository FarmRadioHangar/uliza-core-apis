import { Api } from './helpers';

Api.test.endpoint('/organizations/count').get(result => {

  it('should respond with 200 OK', async () => {
    await result.expect(200);
  });

  it('should return a count of 21', async () => {
    const response = await result;
    response.body.should.have.property('count');
    response.body.count.should.equal(21);
  });

});
