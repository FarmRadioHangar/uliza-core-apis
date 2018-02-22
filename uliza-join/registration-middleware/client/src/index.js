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
  Col,
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
    return (
      <div>
        <Navbar fixedTop>
          <Navbar.Header>
            <Navbar.Brand>
              <a href='/'>
                Uliza Registration
              </a>
            </Navbar.Brand>
          </Navbar.Header>
          <CallListControl />
        </Navbar>
        <Grid style={{marginTop: '68px'}}>
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
