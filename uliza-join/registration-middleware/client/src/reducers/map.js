import { combineReducers } from 'redux';
import * as actions from '../actionTypes';

const DEFAULT_CENTER = {
  lat: 1.4, 
  lng: 32.582520 
};

const center = (state = DEFAULT_CENTER, action) => {
  switch (action.type) {
    case actions.SET_MAP_CENTER:
      return {
        lat: action.lat,
        lng: action.lng
      };
    default:
      return state;
  }
}

const mapReducer = combineReducers({
  center
});

export default mapReducer;
