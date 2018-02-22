import * as api from './api';
import * as actions from './actionTypes';

function assignLocationSuccess(participant) {
  return {
    type: actions.ASSIGN_LOCATION_SUCCESS,
    participant
  };
}

function assignLocationError(error) {
  return {
    type: actions.ASSIGN_LOCATION_ERROR,
    error
  };
}

export function assignLocation(participant, pos) {
  return function(dispatch) {
    dispatch({type: actions.ASSIGN_LOCATION_ASYNC_BEGIN});
    api.assignLocation(participant, pos)
    .then(response => {
      dispatch(assignLocationSuccess(response));
      dispatch(fetchRegistrationCalls());
    })
    .catch(error => {
      dispatch(assignLocationError(error));
      console.error(error);
    });
  }
}

function getCallSuccess(call) {
  return {
    type: actions.GET_CALL_SUCCESS,
    call
  };
}

function getCallError(error) {
  return {
    type: actions.GET_CALL_ERROR,
    error
  };
}

export function getCall(id) {
  return function(dispatch) {
    api.getCall(id)
    .then(call => {
      dispatch(getCallSuccess(call));
    })
    .catch(error => {
      dispatch(getCallError(error));
      console.error(error);
    });
  };
}

function fetchCallsSuccess(calls) {
  return {
    type: actions.FETCH_CALLS_SUCCESS,
    calls
  };
}

function fetchCallsError(error) {
  return {
    type: actions.FETCH_CALLS_ERROR,
    error
  };
}

export function fetchRegistrationCalls() {
  return function(dispatch) {
    dispatch({type: actions.FETCH_CALLS_ASYNC_BEGIN});
    api.fetchRegistrationCalls()
    .then(calls => {
      setTimeout(() => {
        /* Natural lag */
        dispatch(fetchCallsSuccess(calls));
      }, 400);
    })
    .catch(error => {
      setTimeout(() => {
        /* Natural lag */
        dispatch(fetchCallsError(error));
      }, 400);
      console.error(error);
    });
  };
}

export function changeTab(key) {
  return {
    type: actions.CHANGE_TAB,
    key
  };
}

export function setSearchQuery(value) {
  return {
    type: actions.SET_SEARCH_QUERY,
    value
  };
}

export function setLocation(pos) {
  return {
    type: actions.SET_LOCATION,
    lat: pos.lat,
    lng: pos.lng
  };
}

export function setMapCenter(pos) {
  return {
    type: actions.SET_MAP_CENTER,
    lat: pos.lat,
    lng: pos.lng
  };
}
