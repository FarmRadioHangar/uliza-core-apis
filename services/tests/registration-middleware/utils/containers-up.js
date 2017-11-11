var $ = require('./docker');

return Promise.resolve()
.then($.acquireBaseImages)
.then($.buildImage('farmradio/uliza_api', '../../../django-api/'))
.then($.buildImage('farmradio/registration_service', '../../registration-middleware/'))
.then($.buildImage('farmradio/voto_mock_api', '../voto-mock-api/'))
.then($.createContainer('mysql'))
.then($.createContainer('api'))
.then($.createContainer('middleware'))
.then($.createContainer('voto'))
.then($.startContainer('database'))
.then($.startContainer('api'))
.then($.startContainer('voto'))
// Wait for MySQL to accept connections
.then($.pingMysql)                     
.then($.startContainer('middleware' ))
.then($.runMigrations)
.then($.runServer)
.then($.pingApi) 
.catch(console.error);
