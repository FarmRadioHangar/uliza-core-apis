import { combineReducers } from 'redux';
import Fuse         from 'fuse.js';
import * as actions from '../actionTypes';
import locations    from '../../locations.json';

const options = {
  includeMatches: true,
  keys: ['name']
};

const fuse = new Fuse(locations, options);

const query = (state = '', action) => {
  switch (action.type) {
    case actions.SET_SEARCH_QUERY:
      return action.value;
    default:
      return state;
  }
}

const matches = (state = [], action) => {
  switch (action.type) {
    case actions.SET_SEARCH_QUERY:
      if (action.value) {
        return fuse.search(action.value.trim()).map(
          entry => entry.item
        );
      } else {
        return [];
      }
    case actions.SET_LOCATION:
      return [];
    default:
      return state;
  }
}

const position = (state = null, action) => {
  switch (action.type) {
    case actions.SET_LOCATION:
      return {
        lat: action.lat,
        lng: action.lng
      };
    case actions.GET_CALL_SUCCESS:
      return null;
    default:
      return state;
  }
}

const locationReducer = combineReducers({
  query,
  matches,
  position
});

export default locationReducer;
