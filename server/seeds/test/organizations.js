'use strict';

exports.seed = function(knex, Promise) {
  return knex('organizations').del()
    .then(function() {
      return knex('organizations').insert([
        { id: 1, name: 'rowValue1' },
        { id: 2, name: 'rowValue2' },
        { id: 3, name: 'rowValue3' }
      ]);
    });
};
