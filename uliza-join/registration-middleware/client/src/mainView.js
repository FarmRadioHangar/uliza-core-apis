import React         from 'react';
import { changeTab } from './actions';
import { connect }   from 'react-redux';
import { Tabs, Tab, Table } from 'react-bootstrap';

function isAudioResponse(block) {
  return 'Open-Ended Question' === block.block_type 
    && block.response 
    && block.response.open_audio_file;
}

function isMultipleChoice(block) {
  return 'Multiple Choice Question' === block.block_type;
}

const MainView = ({ onSelect, tab, call }) => {
  if (call) {
    const recordings = call.interactions.filter(isAudioResponse);
    const responses = call.interactions.filter(isMultipleChoice);
    return (
      <div>
        <Tabs
          id='main-nav'
          activeKey={tab}
          onSelect={key => onSelect(key)}>
          <Tab eventKey={1} title='Audio responses'>
            {!!recordings.length ? (
              <div>
                <h3>Audio responses</h3>
                <Table striped bordered condensed>
                  <thead>
                    <tr>
                      <th>Question</th>
                      <th>Audio</th>
                    </tr>
                  </thead>
                  <tbody>
                    {recordings.map(entry => 
                      <tr key={entry.block_id}>
                        <td>{entry.title}</td>
                        <td>
                          {entry.response && (
                            <span>
                              <audio controls>
                                <source src={entry.response.open_audio_url} type="audio/wav" />
                                <a href={entry.response.open_audio_url}>Play</a>
                              </audio>
                            </span>
                          )}
                        </td>
                      </tr>
                    )}
                  </tbody>
                </Table>
              </div>
            ) : (
              <div style={{margin: '10px'}}>No audio available.</div>
            )}
          </Tab>
          <Tab eventKey={2} title='Call interaction log'>
            {!!responses.length ? (
              <div>
                <h3>Call interaction log</h3>
                <Table striped bordered condensed>
                  <thead>
                    <tr>
                      <th>Question</th>
                      <th>Answer</th>
                    </tr>
                  </thead>
                  <tbody>
                    {responses.map(entry => 
                      <tr key={entry.block_id}>
                        <td>{entry.title}</td>
                        <td>
                          {entry.response && (
                            <span>
                              {entry.response.choice_name}
                            </span>
                          )}
                        </td>
                      </tr>
                    )}
                  </tbody>
                </Table>
              </div>
            ) : (
              <div style={{margin: '10px'}}>No responses.</div>
            )}
          </Tab>
        </Tabs>
      </div>
    );
  } else {
    return (
      <div>
        No call selected.
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    tab: state.nav.tab,
    call: state.calls.selected
  }
}

const mapDispatchToProps = dispatch => {
  return {
    onSelect: key => {
      dispatch(changeTab(key));
    }
  };
}

const Component = connect(
  mapStateToProps,
  mapDispatchToProps
)(MainView);

export default Component;
