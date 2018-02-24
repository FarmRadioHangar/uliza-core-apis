require('dotenv').config();

var bodyparser  = require('body-parser');
var chalk       = require('chalk');
var express     = require('express');
var requestJson = require('request-json');
var moment      = require('moment');
var url         = require('url');
var path        = require('path');

var app = express();

app.use(bodyparser.urlencoded({extended: true}));
app.use(bodyparser.json());
app.use(express.static('client/dist'));

var SERVER_PORT   
  = process.env.PORT          || 8091;
var ULIZA_API_URL 
  = process.env.ULIZA_API_URL || 'http://dev.uliza.fm/api/v1/';
var VOTO_API_URL  
  = process.env.VOTO_API_URL  || 'https://go.votomobile.org/api/v1/';
var VOTO_API_KEY  
  = process.env.VOTO_API_KEY;
var TEST_SURVEY_PASSWORD = process.env.TEST_SURVEY_PASSWORD;

var MIN_RESCHEDULE_DELAY = process.env.MIN_RESCHEDULE_DELAY || 172800;
var CALL_SCHEDULE_OFFSET = process.env.CALL_SCHEDULE_OFFSET || 600;

var router = express.Router();
var client = requestJson.createClient(ULIZA_API_URL);
var voto   = requestJson.createClient(VOTO_API_URL);

voto.headers['api_key'] = VOTO_API_KEY;

/* Server error exceptions */

function errorMsg(code, error, message) {
  var resp = {
    status: code
  };
  if (error) {
    resp.error = error;
  }
  if (message) {
    resp.message = message;
  }
  return resp;
}

function badRequest(message, error) {
  return errorMsg(400, error || 'badRequest', message);
}

function notFound(message, error) {
  return errorMsg(404, error || 'notFound', message);
}

function badGateway(message, error) {
  return errorMsg(502, error || 'badGateway', message);
}

function internalServerError(message, error) {
  return errorMsg(500, error || 'internalServerError', message);
}

function ulizaError(error) {
  console.error(chalk.redBright('[uliza_error] ') 
    + 'Error connecting to Uliza API: ' + error.code + '.'
  );
  return internalServerError(
    'Error connecting to the Uliza API.'
  );
}

function rejectNonStandard(uri) {
  if ('' !== uri) {
    var part = uri.split('?')[0];
    if ('/' !== part.substr(-1)) {
      throw internalServerError(
        'Error: No trailing slash on uri \'' + ULIZA_API_URL + part + '\''
      );
    }
  }
}

function buildRequest(yield) {
  return new Promise(function(resolve, reject) {
    var callback = function(error, response, body) {
      if (error) {
        return reject(error);
      }
      if (isOk(response.statusCode)) {
        console.log(
          chalk.yellow('[response_code] ') 
          + chalk.green('\u2714 ')
          + chalk.white(response.statusCode)
        );
      } else {
        /* Log body if we get something else than a 2xx response. */
        console.log(
          chalk.redBright('[response_code] ') 
          + chalk.white(response.statusCode)
        );
        console.log(
          chalk.redBright('[response_body] ') 
          + JSON.stringify(response.body)
        );
      }
      resolve({
        all: response,
        body: body
      });
    };
    yield(callback);
  });
}

function isOk(code) {
  return '2' === (code + '')[0];
}

function validate(response, allowed) {
  var code = response.all.statusCode;
  if (!isOk(code) && -1 == allowed.indexOf(code)) {
    console.error(chalk.redBright(
      response.body.message || response.body
    ));
    throw badGateway(
      'serverNon200Response',
      'Server returned a ' + code + ' status code.'
    );
  }
}

function ulizaRequest(uri, respCodes, method, data) {
  rejectNonStandard(uri);
  console.log(
    chalk.magentaBright.bold(method + ' ' + ULIZA_API_URL + uri)
  );
  return buildRequest(function(callback) {
    if ('object' === typeof(data)) {
      /* Log request body for debugging purposes */
      console.log(
        chalk.magentaBright('[request_body] ') + JSON.stringify(data)
      );
      client[method.toLowerCase()](uri, data, callback);
    } else {
      client[method.toLowerCase()](uri, callback);
    }
  })
  .then(function(response) {
    validate(response, respCodes || []);
    return response;
  });
}

function ulizaGet(uri, respCodes) {
  return ulizaRequest(uri, respCodes, 'GET');
}

function ulizaPost(uri, data, respCodes) {
  return ulizaRequest(uri, respCodes, 'POST', data);
}

function ulizaPatch(uri, data, respCodes) {
  return ulizaRequest(uri, respCodes, 'PATCH', data);
}

function votoRequest(uri, respCodes, method, data) {
  console.log(
    chalk.magentaBright(method + ' ' + VOTO_API_URL + uri)
  );
  return buildRequest(function(callback) {
    if ('object' === typeof(data)) {
      /* Log request body for debugging purposes */
      console.log(
        chalk.magentaBright('[request_body] ') + JSON.stringify(data)
      );
    } 
    if (data) {
      voto[method.toLowerCase()](uri, data, callback);
    } else {
      voto[method.toLowerCase()](uri, callback);
    }
  })
  .then(function(response) {
    validate(response, respCodes || []);
    if (isOk(response.all.statusCode) && !response.body.data) {
      console.error(chalk.redBright('[voto_error] ') 
        + 'Missing property \'data\' in VOTO response.' 
      );
      throw badGateway(
        'The VOTO API returned an unexpected response.'
      );
    }
    return {
      all: response.all,
      json: response.body
    };
  });
}

function votoGet(uri, respCodes) {
  return votoRequest(uri, respCodes, 'GET');
}

function votoPost(uri, data, respCodes) {
  return votoRequest(uri, respCodes, 'POST', data);
}

function requireBodyField(request, field) {
  if (!request.body[field]) {
    var msg = 'Field ' + field + ' is required.';
    console.error(chalk.redBright('[bad_webhook_request] ') + msg);
    throw badRequest(msg, 'badWebhookRequest');
  }
}

function requireQueryParam(request, param) {
  if (!request.query[param]) {
    var msg = 'Query parameter ' + param + ' missing.';
    console.error(chalk.redBright('[bad_webhook_request] ') + msg);
    throw badRequest(msg, 'badWebhookRequest');
  }
}

function getParticipant(phone, create) {
  return ulizaGet('participants/?phone_number=' + phone)
  .then(function(response) {
    if (response.body.length) {
      /* A participant already exists. */
      console.log(
        chalk.white('Participant exists.')
      );
      console.log(
        chalk.cyan('[participant_found] ') + JSON.stringify(response.body[0])
      );
      return response.body[0];
    } else {
      /* No participant with this phone number was found. */
      if (!create) {
        throw notFound('Participant not found in the system.');
      }
      console.log(
        chalk.white('No Uliza participant found. Let\'s create one!')
      );
      /* Create the participant! */
      return ulizaPost('participants/', {
        phone_number: phone,
        registration_status: 'NOT_REGISTERED'
      })
      .then(function(response) {
        console.log(
          chalk.cyan('[participant_created] ') + JSON.stringify(response.body)
        );
        return response.body;
      });
    }
  });
}

function scheduleCall(participant, req) {
  var treeId = req.query.tree_id;
  var outgoingCallId = req.body.outgoing_call_id;
  /* Is the participant already registered? */
  if ('REGISTERED' == participant.registration_status) {
    return {
      action: 'NONE',
      reason: 'ALREADY_REGISTERED'
    };
  }
  /* Have they previously declined to register with the service? */
  if ('DECLINED' == participant.registration_status) {
    return {
      action: 'NONE',
      reason: 'REGISTRATION_DECLINED'
    };
  }
  return ulizaGet('participants/' + participant.id + '/registration_call/', [404])
  .then(function(response) {
    /* Was a registration call found for this participant? */
    if (404 != response.all.statusCode) {
      console.log(
        chalk.cyan('[registration_call_found] ') + JSON.stringify(response.body)
      );
      if (response.body.schedule_time) {
        console.log(
          chalk.white('schedule_time = ' + response.body.schedule_time)
        );
        var diff = moment().diff(moment.utc(response.body.schedule_time), 'seconds');
        /* Is a registration call already due for the participant? */
        if (diff < 0) {
          return {
            action: 'NONE',
            reason: 'PRIOR_CALL_SCHEDULED'
          };
        }
        /* Did a call to the participant take place recently? */
        if (diff < MIN_RESCHEDULE_DELAY) {
          return {
            action: 'NONE',
            reason: 'TOO_SOON'
          };
        }
      } else {
        console.log(
          chalk.white('schedule_time = null')
        );
        if ('' + outgoingCallId == '' + response.body.voto_survey_call_id) {
          /* A thread created by another response is processing this call. */
          return {
            action: 'NONE',
            reason: 'RACE_CONDITION'
          };
        }
      }
    }
    var time = moment.utc().add(CALL_SCHEDULE_OFFSET, 'seconds');
    var regCallId;
    return ulizaPost('registration_calls/', {
      participant: participant.id,
      schedule_time: null, /* Added later */
      voto_call_id: null,  /* Added later */
      voto_survey_call_id: outgoingCallId,
      voto_tree_id: treeId,
      phone_number: participant.phone_number,
      call_status: 'SCHEDULED'
    })
    .then(function(response) {
      console.log(
        chalk.cyan('[registration_call_created] ') + JSON.stringify(response.body)
      );
      regCallId = response.body.id;
      return votoPost('outgoing_calls/', {
        send_to_phones: participant.phone_number,
        tree_id: treeId,
        webhook_url: url.format({
          protocol: req.protocol,
          host: req.get('host'),
          pathname: 'call_status_updates'
        }),
        webhook_method: 'POST',
        schedule_type: 'fixed',
        schedule_date: time.format('YYYY-MM-DD'),
        schedule_time: time.format('HH:mm')
      })
    })
    .then(function(response) {
      console.log(
        chalk.greenBright('[call_id] ') 
        + chalk.yellowBright(response.json.data)
      );
      /* Update the registration call with the returned VOTO call id. */
      return ulizaPatch('registration_calls/' + regCallId + '/', {
        schedule_time: time.format('YYYY-MM-DD HH:mm'),
        voto_call_id: response.json.data
      })
      .then(function(response) {
        response.body.phone_number = participant.phone_number;
        return {
          action: 'REGISTRATION_CALL_SCHEDULED',
          registration_call: response.body
        };
      });
    });
  });
}

function getVotoSubscriber(id) {
  return votoGet('subscribers/' + id, [404])
  .then(function(response) {
    if (404 == response.all.statusCode) {
      console.error(chalk.redBright('[bad_webhook_request] ') 
        + 'No VOTO subscriber with a matching id found' 
      );
      throw badRequest('Invalid VOTO subscriber ID.');
    }
    return response.json.data.subscriber;
  });
}

function registerParticipant(subscriber, req) {
  var callId = req.body.outgoing_call_id;
  var deliveryLogId = req.body.delivery_log_id;
  var stringProps = function(attributes) {
    var obj = {};
    for (var key in attributes) {
      var val = attributes[key];
      if ('string' === typeof(val)) {
        obj['attribute__' + key.replace(/[^\w]/g, '')] = val;
      }
    }
    return obj;
  };
  return votoGet('outgoing_calls/' + callId)
  .then(function(response) {
    var treeId = response.json.data.outgoing_call.tree_id;
    return votoGet('trees/' + treeId + '/delivery_logs/' + deliveryLogId);
  })
  .then(function(response) {
    return ulizaPatch('registration_calls/voto_call/' + callId + '/', {
      interactions: JSON.stringify(response.json.data.interactions),
      call_status: 'COMPLETE'
    });
  })
  .then(function(response) {
    return getParticipant(subscriber.phone)
  })
  .then(function(participant) {
    return ulizaPatch('participants/' + participant.id + '/', {
      registration_status: 'REGISTERED',
      attributes: stringProps(subscriber.properties)
    });
  });
}

router.post('/responses', function(req, res) {
  return Promise.resolve()
  .then(function() {
    console.log('====================');
    console.log(chalk.cyan('[voto_response_hook] ') + JSON.stringify(req.body));
    requireQueryParam(req, 'tree_id');
    requireBodyField(req, 'subscriber_phone');
    requireBodyField(req, 'outgoing_call_id');
    requireBodyField(req, 'delivery_status');
    if (req.body.delivery_status === 3 || req.body.delivery_status === '3') {
      /* Call in progress */
      return getParticipant(req.body.subscriber_phone, true)
      .then(function(participant) {
        return scheduleCall(participant, req);
      })
      .then(function(response) {
        res.json(response);
        console.log(
          chalk.greenBright('[json_response_sent] ') + JSON.stringify(response)
        );
      })
    } else {
      res.json({
        action: 'NO_ACTION',
        reason: 'DELIVERY_STATUS_MISMATCH'
      });
    }
  })
  .catch(function(error) {
    var response = {error: error.error};
    if (error.message) {
      response.message = error.message;
    }
    res.status(error.status || 500);
    res.json(response);
    console.error(chalk.redBright(JSON.stringify(response)));
  });
});

router.post('/call_status_updates', function(req, res) {
  return Promise.resolve()
  .then(function() {
    console.log('=========================');
    console.log(chalk.cyan('[voto_call_status_update] ') 
      + JSON.stringify(req.body));
    requireBodyField(req, 'outgoing_call_id');
    requireBodyField(req, 'delivery_log_id');
    requireBodyField(req, 'delivery_status');
    requireBodyField(req, 'subscriber_id');
    requireBodyField(req, 'subscriber_phone');
    if (req.body.delivery_status === 6 || req.body.delivery_status === '6') {
      return getVotoSubscriber(req.body.subscriber_id)
      .then(function(subscriber) {
        console.log(
          chalk.cyan('[voto_subscriber_found] ') + JSON.stringify(subscriber)
        );
        if (subscriber.phone != req.body.subscriber_phone) {
          console.warn(
            chalk.red(
              'Subscriber phone doesn\'t match request subscriber_phone field. Running in test mode?'
            )
          );
          subscriber.phone = req.body.subscriber_phone;
        }
        return registerParticipant(subscriber, req)
        .then(function(response) {
          console.log(
            chalk.cyan('[patched_participant] ') + JSON.stringify(response.body)
          );
          res.json({
            action: 'REGISTRATION_COMPLETE',
            data: response.body
          });
        })
      })
    } else {
      /* Delivery status != 6; do nothing */
      res.json({
        action: 'NO_ACTION',
        reason: 'CALL_NOT_COMPLETE'
      });
    }
  })
  .catch(function(error) {
    var response = {error: error.error};
    if (error.message) {
      response.message = error.message;
    }
    res.status(error.status || 500);
    res.json(response);
    console.error(chalk.redBright(JSON.stringify(response)));
  });
});

router.post('/schedule_survey', function(req, res) {
  requireBodyField(req, 'number');
  requireBodyField(req, 'password');
  if (TEST_SURVEY_PASSWORD === req.body.password) {
    votoPost('outgoing_calls/', {
      send_to_phones: req.body.number,
      survey_id: '210929',
      webhook_url: url.format({
        protocol: 'https',
        host: req.get('host'),
        pathname: 'responses',
        query: {
          'tree_id': 22391
        }
      }),
      webhook_method: 'POST'
    }).then(function(response) {
      res.json(response.json);
    });
  } else {
    res.json({message: 'INVALID_PASSWORD'});
  }
});

ulizaGet('')
.catch(function(error) {
  console.error('Failed connecting to Uliza API on ' + ULIZA_API_URL + '.');
  process.exit(1);
})
.then(function() {
  console.log('Uliza API connection OK.');
  return votoGet('languages/');
})
.catch(function(error) {
  console.error('Failed connecting to VOTO API on ' + VOTO_API_URL + '.');
  process.exit(1);
})
.then(function() {
  console.log('VOTO API connection OK.');
  app.use(router);
  app.listen(SERVER_PORT);
  console.log(
    chalk.bold.yellow(
      'Uliza Join registration server listening on port '
      + SERVER_PORT
    )
  );
});
