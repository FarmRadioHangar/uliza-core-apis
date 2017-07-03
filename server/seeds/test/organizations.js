'use strict';

exports.seed = function(knex, Promise) {
  return knex('organizations')
    .del()
    .then(function() {
      return knex('organizations').insert([
        { id: 1,  name: 'Universal Exports' },
        { id: 2,  name: 'Mystery Incorporated' },
        { id: 3,  name: 'Lunar Industries Ltd.' },
        { id: 4,  name: 'Tyrell Corporation' },
        { id: 5,  name: 'Zorin Industries' },
        { id: 6,  name: 'U.N.C.L.E.' },
        { id: 7,  name: 'Reynholm Industries' },
        { id: 8,  name: 'IZON Research Facilities' },
        { id: 9,  name: 'Bluebook' },
        { id: 10, name: 'Omni Presents' },
        { id: 11, name: 'Bendini, Lambert & Locke' }
      ]);
    });
};
