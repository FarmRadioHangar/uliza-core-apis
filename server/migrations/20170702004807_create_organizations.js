'use strict';

exports.up = function(knex, Promise) {
  return Promise.all([
    knex.schema.createTable('organizations', function(table) {
      table.increments();
      table.string('name').notNullable();
      table.timestamp('created_at').defaultTo(knex.fn.now());
      table.timestamp('updated_at').defaultTo(knex.fn.now());
    })
  ]);
};

exports.down = function(knex, Promise) {
  return Promise.all([
    knex.schema.dropTable('organizations')
  ]); 
};
