var $ = require('./docker');

module.exports = function() {
  return Promise.resolve()
  .then($.stopContainer('middleware'))
  .then($.removeContainer('middleware'))
  .then($.stopContainer('voto'))
  .then($.removeContainer('voto'))
  .then($.stopContainer('api'))
  .then($.removeContainer('api'))
  .then($.stopContainer('database'))
  .then($.removeContainer('database'))
  .catch(console.error);
}
