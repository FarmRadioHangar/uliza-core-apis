exports.up = (pg) => {
  pg.createType({
    schema: 'farmradio_api', 
    name: 'registration_event_type'
  }, [
    'REGISTRATION_CALL_SCHEDULED', 
    'REGISTRATION_DECLINED', 
    'REGISTRATION_COMPLETE'
  ]);
  pg.createTable({
    schema: 'farmradio_api', 
    name: 'participant_registration_status_log'
  }, {
    id: { 
      type: 'serial', 
      primaryKey: true 
    },
    participant_id: {
      type: 'int',
      notNull: true,
      references: 'farmradio_api.participants(id)',
      onDelete: ['cascade']
    },
    registration_call_id: {
      type: 'int',
      notNull: true,
      references: 'farmradio_api.registration_calls(id)'
    },
    event_type: {
      type: 'farmradio_api.registration_event_type',
      notNull: true
    },
    created_at: {
      type: 'timestamptz',
      notNull: true,
      default: pg.func('CURRENT_TIMESTAMP')
    }
  });
  pg.createIndex({
    schema: 'farmradio_api', 
    name: 'participant_registration_status_log'
  }, 'participant_id', {
    name: 'participant_registration_status_log_participant_id_index'
  });
  pg.createIndex({
    schema: 'farmradio_api', 
    name: 'participant_registration_status_log'
  }, 'registration_call_id', {
    name: 'participant_registration_status_log_registration_call_id_index'
  });
};

exports.down = (pg) => {
  pg.dropTable({
    schema: 'farmradio_api',
    name: 'participant_registration_status_log'
  });
  pg.dropType({
    schema: 'farmradio_api', 
    name: 'registration_event_type'
  });
};
