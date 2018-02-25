const ULIZA_API_URL = window.__ULIZA_API_URL__;

console.log(ULIZA_API_URL);

export function fetchRegistrationCalls() {
  return fetch(`${ULIZA_API_URL}/registration_calls/missing_location/`)
  .then(response => {
    return response.json();
  });
}

function parseInteractions(call) {
  if (!call.interactions) {
    return [];
  }
  try {
    return JSON.parse(call.interactions);
  } catch(e) {
    console.warn(`Bad JSON fromat in call id=${id}`);
    return [];
  }
}

export function getCall(id) {
  return fetch(`${ULIZA_API_URL}/registration_calls/${id}/`)
  .then(response => {
    return response.json();
  })
  .then(call => {
    return {
      ...call,
      interactions: parseInteractions(call)
    };
  });
}

export function assignLocation(participant, pos) {
  return fetch(`${ULIZA_API_URL}/participants/${participant}/`, {
    headers: {
      'Accept': 'application/json',
      'Content-Type': 'application/json'
    },
    method: 'PATCH',                                                              
    body: JSON.stringify({'location': `${pos.lat}:${pos.lng}`})
  })
  .then(response => {
    return response.json();
  });
}
