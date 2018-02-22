import React       from 'react';
import { connect } from 'react-redux';
import { fetchRegistrationCalls, getCall } from './actions';
import { Button, FormControl, FormGroup, Navbar } from 'react-bootstrap';

const CallListControl = ({ onSelect, onRefresh, calls, state }) => {
  switch (state) {
    case 'FETCHING': 
      return (
        <Navbar.Collapse>
          <Navbar.Text>
            {'Loading registration calls from Uliza.'}
          </Navbar.Text>
        </Navbar.Collapse>
      );
    case 'LOADED':
      return (
        <div>
          {calls.length ? (
            <Navbar.Form pullLeft>
              <FormGroup controlId='registration-calls-select'>
                <FormControl 
                  onChange={e => {
                    const val = e.target.value;
                    if ('select' != val) {
                      onSelect(val);
                    }
                  }} 
                  componentClass='select' 
                  placeholder='select'>
                  <option value='select'>Registration calls</option>
                  {calls.map((call, i) => 
                    <option 
                      key={i}
                      value={call.id}>
                      {call.phone_number}
                    </option>
                  )}
                </FormControl>
                {' '}
                <Button onClick={e => { onRefresh(); }}>Refresh</Button>
              </FormGroup>
            </Navbar.Form>
          ) : (
            <Navbar.Collapse>
              <Navbar.Text>
                {'No registration calls.'}
              </Navbar.Text>
              <Navbar.Form pullLeft>
                <FormGroup>
                  <Button onClick={e => { onRefresh(); }}>Refresh</Button>
                </FormGroup>
              </Navbar.Form>
            </Navbar.Collapse>
          )} </div>
      );
    case 'ERROR':
      return (
        <Navbar.Collapse>
          <Navbar.Text>
            {'Error connecting to Uliza.'}
          </Navbar.Text>
          <Navbar.Form pullLeft>
            <FormGroup>
              <Button onClick={e => { onRefresh(); }}>Retry</Button>
            </FormGroup>
          </Navbar.Form>
        </Navbar.Collapse>
      );
    default: 
      return <div />;
  }
}

const mapStateToProps = state => {
  return {
    calls: state.calls.list,
    state: state.calls.fetchState
  };
}

const mapDispatchToProps = dispatch => {
  return {
    onSelect: id => {
      dispatch(getCall(id));
    },
    onRefresh: () => {
      dispatch(fetchRegistrationCalls());
    }
  };
}

const Component = connect(
  mapStateToProps, 
  mapDispatchToProps
)(CallListControl);

export default Component;
