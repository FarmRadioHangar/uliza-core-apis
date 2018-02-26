import React           from 'react';
import ReactDOM        from 'react-dom';
import autoBind        from 'react-autobind';
import reducers        from './reducers/root';
import Map             from './map';
import NameSearch      from './nameSearch';
import MainView        from './mainView';
import Notifications   from './notifications';
import CallListControl from './callListControl';
import { fetchRegistrationCalls } from './actions';
import { createStore, applyMiddleware } from 'redux';
import { Provider }    from 'react-redux';
import thunk           from 'redux-thunk';

import { 
  Button,
  Col,
  ControlLabel,
  Form,
  FormControl,
  FormGroup,
  Grid,
  Navbar,
  Row
} from 'react-bootstrap';

const store = createStore(
  reducers,
  applyMiddleware(thunk)
);

store.dispatch(fetchRegistrationCalls());

class App extends React.Component {
  render() {
    let phoneNumberInput, passwordInput;
    return (
      <div>
        <Navbar fixedTop>
          <Navbar.Header>
            <Navbar.Brand>
              <a href='/'>
                Uliza Join
              </a>
            </Navbar.Brand>
          </Navbar.Header>
          <CallListControl />
        </Navbar>
        <Grid style={{marginTop: '68px'}}>
          {/* temp. test form */}
          <Row>
            <Form inline style={{padding: '20px', border: '1px solid #a8a8a8'}}>
              <ControlLabel style={{marginRight: '20px'}}>DEMO CALL:</ControlLabel>
              <FormGroup controlId='survey-phone-number'>
                <ControlLabel>Phone number</ControlLabel>{' '}
                <FormControl type='text' inputRef={ref => { phoneNumberInput = ref; }} />
              </FormGroup>{' '}
              <FormGroup controlId='survey-password'>
                <ControlLabel>Password</ControlLabel>{' '}
                <FormControl type='password' inputRef={ref => { passwordInput = ref; } }/>
              </FormGroup>{' '}
              <Button type='submit' onClick={e => {
                e.preventDefault();
                fetch(window.location + 'schedule_survey/', {
                  headers: {
                    'Accept': 'application/json',
                    'Content-Type': 'application/json'
                  },
                  method: 'POST',
                  body: JSON.stringify({
                    number: phoneNumberInput.value,
                    password: passwordInput.value 
                  })
                })
                .then(response => { return response.json(); })
                .then(json => { console.log(json); });
                phoneNumberInput.value = '';
                passwordInput.value = '';
              }}>Schedule test survey call</Button>
            </Form>
          </Row>
          {/* /temp. test form */}
          <Row>
            <Notifications />
          </Row>
          <Row>
            <Col sm={8}>
              <Row>
                <Col sm={12}>
                  <Map />
                  <MainView />
                </Col>
              </Row>
            </Col>
            <Col sm={4}>
              <NameSearch />
            </Col>
          </Row>
        </Grid>
      </div>
    );
  }
}

ReactDOM.render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('app')
);
