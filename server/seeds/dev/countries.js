'use strict';

exports.seed = function(knex, Promise) {
  return knex('countries')
    .del()
    .then(function() {
      return knex('countries').insert([
        { 
          id: 1,
          name: 'Tanzania',
          iso_code: 'TZ'
        },
        { 
          id: 2,
          name: 'Uganda',
          iso_code: 'UG'
        },
        { 
          id: 3,
          name: 'Burkina-Faso',
          iso_code: 'BF'
        },
        { 
          id: 4,
          name: 'Malawi',
          iso_code: 'MW'
        },
        { 
          id: 5,
          name: 'Ghana',
          iso_code: 'GH'
        },
        { 
          id: 6,
          name: 'Mozambique',
          iso_code: 'MZ'
        }
      ])
    });
};
