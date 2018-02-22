import { combineReducers } from 'redux';
import * as actions from '../actionTypes';

const tab = (state = 1, action) => {
  switch (action.type) {
    case actions.CHANGE_TAB:
      return action.key;
    case actions.SET_LOCATION:
      return 1;
    default:
      return state;
  }
}

const navReducer = combineReducers({
  tab
});

export default navReducer;
