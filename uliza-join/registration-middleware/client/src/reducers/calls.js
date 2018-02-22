import { combineReducers } from 'redux';
import * as actions from '../actionTypes';

const list = (state = [], action) => {
  switch (action.type) {
    case actions.FETCH_CALLS_SUCCESS:
      return action.calls;
    default:
      return state;
  }
}

const selected = (state = null, action) => {
  switch (action.type) {
    case actions.GET_CALL_SUCCESS:
      return action.call;
    case actions.ASSIGN_LOCATION_SUCCESS:
      return null;
    default:
      return state;
  }
}

const fetchState = (state = 'INIT', action) => {
  switch (action.type) {
    case actions.FETCH_CALLS_ASYNC_BEGIN:
      return 'FETCHING';
    case actions.FETCH_CALLS_SUCCESS:
      return 'LOADED';
    case actions.FETCH_CALLS_ERROR:
      return 'ERROR';
    default:
      return state;
  }
}

const callsReducer = combineReducers({
  list,
  selected,
  fetchState
});

export default callsReducer;
