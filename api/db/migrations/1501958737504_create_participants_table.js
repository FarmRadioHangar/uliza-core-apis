exports.up = (pg) => {
  pg.createType({
    schema: 'farmradio_api', 
    name: 'registration_status'
  }, [
    'NOT_REGISTERED', 
    'REGISTERED',
    'DECLINED'
  ]);
  pg.createTable({
    schema: 'farmradio_api', 
    name: 'participants'
  }, {
    id: { 
      type: 'serial', 
      primaryKey: true 
    },
    phone_number: {
      type: 'varchar',
      notNull: true
    },
    registration_status: {
      type: 'farmradio_api.registration_status',
      notNull: true
    },
    registration_call_id: {
      type: 'int',
      notNull: false,
      references: 'farmradio_api.registration_calls(id)'
    },
    created_at: {
      type: 'timestamptz',
      notNull: true,
      default: pg.func('CURRENT_TIMESTAMP')
    }
  });
  pg.createIndex({
    schema: 'farmradio_api', 
    name: 'participants'
  }, 'phone_number', {
    unique: true,
    name: 'farmradio_api_participants_phone_number_unique_index'
  });
  pg.createIndex({
    schema: 'farmradio_api', 
    name: 'participants'
  }, 'registration_call_id', {
    name: 'farmradio_api_participants_registration_call_id_index'
  });
};

exports.down = (pg) => {
  pg.dropTable({
    schema: 'farmradio_api',
    name: 'participants'
  });
  pg.dropType({
    schema: 'farmradio_api', 
    name: 'registration_status'
  });
};
