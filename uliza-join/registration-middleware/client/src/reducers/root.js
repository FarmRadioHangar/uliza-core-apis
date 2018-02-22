import { combineReducers } from 'redux';
import locationReducer from './location';
import mapReducer from './map';
import navReducer from './nav';
import callsReducer from './calls';

const rootReducer = combineReducers({
  location: locationReducer,
  map: mapReducer,
  nav: navReducer,
  calls: callsReducer
});

export default rootReducer;
