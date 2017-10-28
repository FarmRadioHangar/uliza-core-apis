require('dotenv').config();

var request = require('request');
var qs      = require('qs');

function serialize(obj) {
  var str = [];
  for (var p in obj) {
    if (obj.hasOwnProperty(p)) {
      str.push(encodeURIComponent(p) + '=' + encodeURIComponent(obj[p]));
    }
  }
  return str.join('&');
}

var testdata_0 = {
  question_id: "127375", 
  survey_id: "89324", 
  voto_id: "44", 
  response_type: "1", 
  content_type: "1", 
  poll_id: "213", 
  delivery_log_id: "832", 
  choice_id: "1", 
  subscriber_id: "232", 
  subscriber_phone: "+255678647268", 
  question_title: "Who was the first man to set foot on the moon?", 
  choice_name: "Neil Armstrong", 
  date_received: "2017-07-24T18:13:51Z"
};

var testdata_1 = {
  subscriber_phone: "255678647268",
  subscriber_id: "3",
  delivery_status: "6"
};

var responses = function(data) {
  request.post({
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      'Accept': 'application/json'
    },
    url: process.env.REG_SERVICE_URL + '/responses',
    body: serialize(data)
  }, function(error, response, body) {
    console.log(body);
  });
}

var callStatusUpdates = function(data) {
  request.post({
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      'Accept': 'application/json'
    },
    url: process.env.REG_SERVICE_URL + '/call_status_updates',
    body: serialize(data)
  }, function(error, response, body) {
    console.log(body);
  });
}

module.exports = {
  responses: responses,
  callStatusUpdates: callStatusUpdates,
  testdata_0: testdata_0,
  testdata_1: testdata_1
};
