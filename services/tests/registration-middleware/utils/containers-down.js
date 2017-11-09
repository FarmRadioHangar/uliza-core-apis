var $ = require('./docker');

return Promise.resolve()
.then($.stopContainer('middleware'))
.then($.removeContainer('middleware'))
.then($.stopContainer('api'))
.then($.removeContainer('api'))
.then($.stopContainer('database'))
.then($.removeContainer('database'))
.catch(console.error);
