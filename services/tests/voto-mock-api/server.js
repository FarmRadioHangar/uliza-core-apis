require('dotenv').config();

var express    = require('express');
var bodyParser = require('body-parser');

var app = express();

app.use(bodyParser.urlencoded({extended: true}));
app.use(bodyParser.json());

var port = process.env.PORT || 8089;

var router = express.Router();

app.post('/', function(req, res) {
  res.json({ 
    message: 'Okay!' 
  });   
});

// https://go.votomobile.org/apidoc/subscribers.html#list-subscriber-details
router.get('/subscribers/:id', function(req, res) {
  console.log(req.query);
  res.json({ 
    "status": 200, 
    "code": 1000, 
    "data": { 
      "subscriber": { 
        "id": "373751", 
        "receive_sms": "1", 
        "receive_voice": "1", 
        "receive_data": "0", 
        "receive_ussd": "0", 
        "phone": "255786082881", 
        "active": "1", 
        "start_date": "2014-03-12", 
        "language_id": "200715", 
        "is_test_subscriber": "1", 
        "group_ids": "200605, 201212, 222874", 
        "name": "Bart Sullivan", 
        "location": "Arusha", 
        "comments": "For the switch board", 
        "properties": { 
          "name": "Bart Sullivan", 
          "location": "Arusha", 
          "comments": "For the switch board", 
          "gender": "Female", 
          "age_group": "35-50", 
          "occupation": "Farmer", 
          "zone": null, 
          "region": null, 
          "district": null, 
          "registered": "true", 
          "registration": null, 
          "registration_status": null, 
          "age": null, 
          "yes": null, 
          "no": null, 
          "wilaya": null, 
          "kijiji": null, 
          "register": null 
        } 
      } 
    }, 
    "message": "Subscriber details fetched successfully", 
    "more_info": "", 
    "pagination": null
  });   
});

// https://go.votomobile.org/apidoc/outgoing_calls.html#create-an-outgoing-call
router.post('/outgoing_calls', function(req, res) {
  //res.json({
  //  "status": 200,
  //  "message": "Outgoing call qeued successfully",
  //  "data": {
  //    id: 2345554
  //  }
  //});
  res.json({
    "status": 200,
    "message": "Outgoing call qeued successfully",
    "data": 2345555 
  });
});

// https://go.votomobile.org/apidoc/trees_results.html#get-tree-interactions-responses-for-one-delivery-log
router.get('/trees/:tree_id/delivery_logs/:log_id', function(req, res) {
  res.json({
    "status": 200,
    "code": 1000,
    "data": {
      "delivery_log": {
        "id": "183243779",
        "subscriber_id": "125279980",
        "outgoing_call_id": "2023285",
        "incoming_call_id": null,
        "queued_on": "2017-11-10 09:22:11",
        "delivery_status": "7",
        "seconds_completed": "25.1",
        "start_timestamp": "2017-11-10 16:31:30",
        "end_timestamp": "2017-11-10 16:31:55",
        "content_type": "1",
        "language_id": "201194",
        "hangup_cause": "NORMAL_CLEARING"
      },
      "tree": {
        "id": "19278",
        "starting_block_id": "7855893",
        "num_blocks":"37"
      },
      "interactions": [
        {
          "block_interaction_id": "342717997",
          "block_id": "7855893",
          "block_type": "Edit Group Membership",
          "number_of_repeats": "0",
          "entry_at": "2017-11-10 16:31:30",
          "exit_at": "2017-11-10 16:31:30"
        },
        {
          "block_interaction_id": "342717998",
          "block_id": "7855865",
          "block_type": "Message",
          "number_of_repeats": "0",
          "entry_at": "2017-11-10 16:31:30",
          "exit_at": "2017-11-10 16:31:55",
          "title": "Introduction"
        }
      ]
    },
    "message": "Delivery Log 183243779 Tree 19278 Interactions Fetched Successfully",
    "more_info": "",
    "pagination": null
  })
});

// https://go.votomobile.org/apidoc/delivery_logs.html#get-delivery-logs-for-an-outgoing-call
router.get('/outgoing_calls/:id/delivery_logs', function(req, res) {
  res.json({
    "status": 200,
    "code": 1000,
    "data": {
      "delivery_logs": [
        {
          "id": "187231730",
          "incoming_call_id": null,
          "outgoing_call_id": "2034316",
          "seconds_completed": "185.02",
          "message_seconds_total": null,
          "retried_times": null,
          "start_timestamp": "2017-11-17 13:38:56",
          "end_timestamp": "2017-11-17 13:42:01",
          "message_start_timestamp": null,
          "message_end_timestamp": null,
          "message_percent_listened": null,
          "survey_questions_answered": "0",
          "survey_questions_total": null,
          "total_call_attempts": "1",
          "last_call_attempt": "2017-11-17 13:38:46",
          "survey_id": null,
          "message_id": null,
          "tree_id": "19278",
          "subscriber_id": "125279980",
          "language_id": "201194",
          "phone": "256784224203",
          "survey": null,
          "message": null,
          "subscriber": {
            "id": "125279980",
            "phone": "256784224203",
            "language_id": "201194",
            "delivery_logs": "https://go.votomobile.org/api/v1/subscribers/125279980/delivery_logs"
          }
        }
      ]
    },
    "message": "Delivery logs fetched successfully."
  });
});

// https://go.votomobile.org/apidoc/outgoing_calls.html#list-details-of-an-outgoing-call
router.get('/outgoing_calls/:id', function(req, res) {
  res.json({
    "status": 200,
    "code": 1000,
    "data": {
      "outgoing_call": {
        "id": "2345554",
        "schedule_type": "1",
        "send_to_all": "1",
        "has_sms": "1",
        "has_voice": "0",
        "message_id": null,
        "survey_id": "202385",
        "tree_id": null,
        "poll_id": "233077",
        "routine_days": null,
        "scheduled_date": "2014-09-15",
        "queued_on": "2014-09-15 14:02:21",
        "retry_attempts_short": "3",
        "retry_attempts_long": "1",
        "retry_delay_short": "1",
        "retry_delay_long": "60",
        "retry_count_short": "0",
        "retry_count_long": "0",
        "created_at": "2014-09-15 14:02:21",
        "updated_at": "2015-09-15 18:50:36",
        "webhook": {
          "url": "https://someapi.io/webhooks/requests",
          "method": "POST",
          "secret": "my-app-secret"
        }
      }
    },
    "message": "Outgoing Call Details Fetched successfully",
    "more_info": ""
  });
});

router.get('/languages', function(req, res) {
  res.json({status: 200, code: 1000, data: { languages: [ { id: "201194", name: "English", abbreviation: "EN" }, { id: "203161", name: "Rukiiga", abbreviation: "RU" }, { id: "203162", name: "Luo", abbreviation: "LU" }, { id: "203163", name: "Luganda", abbreviation: "LG" } ] }, message: "Languages fetched successfully ", more_info: "", pagination: null, url: "https://go.votomobile.org/api/v1/languages?api_key=7dc05a5fb33f9251977e206b3"});
});

app.use('/api/v1', router);
app.listen(port);

console.log('Mock API server running on ' + port);
