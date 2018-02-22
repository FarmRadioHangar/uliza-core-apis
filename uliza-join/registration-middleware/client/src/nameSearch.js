import React       from 'react';
import { connect } from 'react-redux';
import { assignLocation, setSearchQuery, setLocation, setMapCenter } from './actions';

import { 
  Button, 
  FormControl, 
  ListGroup, 
  ListGroupItem,
  Well
} from 'react-bootstrap';

const NameSearch = ({ onAssignLocation, onChange, onSelect, query, matches, position, call }) => {
  let input = null;
  return (
    <div>
      <div>
        {call && (
          <div>
            <h4>Call details</h4>
            <Well>
              <dl style={{margin: 0}}>
                <dt>Phone number</dt>
                <dd>{call.phone_number}</dd>
                <dt>Schedule time</dt>
                <dd>{call.schedule_time}</dd>
                <dt>Viamo tree id</dt>
                <dd>{call.voto_tree_id}</dd>
                <dt>Viamo call id</dt>
                <dd>{call.voto_call_id}</dd>
              </dl>
            </Well>
          </div>
        )}
        <div>
          {position && (
            <div>
              <h4>Position coordinates</h4>
              <dl>
                <dt>Lat</dt>
                <dd>{position.lat}</dd>
                <dt>Lng</dt>
                <dd>{position.lng}</dd>
              </dl>
              {call && (
                <Button 
                  bsStyle='primary' 
                  bsSize='large' 
                  onClick={e => { onAssignLocation(call.participant, position); }}
                  block>
                  {'Assign this location to participant'}
                </Button>
              )}
              <hr />
            </div>
          )}
        </div>
        <h4>Location name search</h4>
        <FormControl 
          placeholder='Type the name of a town, city or village'
          type='text'
          inputRef={ref => { input = ref; }}
          onChange={e => { onChange(e.target.value); }} />
      </div>
      <div style={{margin: '10px'}}>
        <ListGroup>
          {matches.map((item, i) => 
            <ListGroupItem 
              key={i} 
              href='#'
              onClick={e => {
                e.preventDefault();
                onSelect(item);
                input.value = '';
              }}>
              {item.name}
            </ListGroupItem>
          )}
        </ListGroup>
      </div>
    </div>
  );
}

const mapStateToProps = state => {
  return {
    call: state.calls.selected,
    ...state.location
  };
}

const mapDispatchToProps = dispatch => {
  return {
    onChange: value => {
      dispatch(setSearchQuery(value));
    },
    onSelect: item => {
      dispatch(setLocation(item));
      dispatch(setMapCenter(item));
    },
    onAssignLocation: (participant, pos) => {
      dispatch(assignLocation(participant, pos));
    }
  };
}

const Component = connect(
  mapStateToProps,
  mapDispatchToProps
)(NameSearch);

export default Component;
