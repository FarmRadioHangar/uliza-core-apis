'use strict';

exports.seed = function(knex, Promise) {
  return knex('organizations')
    .del()
    .then(function() {
      return Promise.all([
        knex('organizations').insert([
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
          { id: 11, name: 'Bendini, Lambert & Locke' },
          { id: 12, name: 'Omni Consumer Products' },
          { id: 13, name: 'Destroido Corp.' },
          { id: 14, name: 'Icebergs International' },
          { id: 15, name: 'Vandelay Industries' },
          { id: 16, name: 'Stark Industries' },
          { id: 17, name: 'Burns Industries' },
          { id: 18, name: 'Cyberdyne Systems' },
          { id: 19, name: 'Los Pollos Hermanos' },
          { id: 20, name: 'Umbrella Corporation' },
          { id: 21, name: 'Planet Express, Inc.' }
        ])
      ]);
    });
};
