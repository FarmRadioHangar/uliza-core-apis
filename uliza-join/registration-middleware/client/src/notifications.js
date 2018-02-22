import React                      from 'react';
import { connect }                from 'react-redux';
import { Button, Alert }          from 'react-bootstrap';
import { fetchRegistrationCalls } from './actions';

const Notifications = ({ onClick, fetchState }) => {
  return (
    <span>
      {'ERROR' === fetchState && (
        <Alert bsStyle='danger'>
          {'Could not connect to the Uliza API.'}
        </Alert>
      )}
    </span>
  );
}

const mapStateToProps = state => {
  return {
    fetchState: state.calls.fetchState
  };
}

const mapDispatchToProps = dispatch => {
  return {
    onClick: e => {
      dispatch(fetchRegistrationCalls());
    }
  };
}

const Component = connect(
  mapStateToProps,
  mapDispatchToProps
)(Notifications);

export default Component;
