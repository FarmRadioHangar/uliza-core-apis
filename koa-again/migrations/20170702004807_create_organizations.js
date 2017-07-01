'use strict';

exports.up = function(knex, Promise) {
  knex.schema.createTable('organizations', function(table) {
    table.increments();
    table.string('name');
    table.timestamps();
  });
};

exports.down = function(knex, Promise) {
  knex.schema.dropTable('organizations');
};
