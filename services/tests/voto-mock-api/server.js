require('dotenv').config();

var express    = require('express');
var bodyParser = require('body-parser');

var app = express();

app.use(bodyParser.urlencoded({extended: true}));
app.use(bodyParser.json());

var port = process.env.PORT || 8089;

var router = express.Router();

router.get('/', function(req, res) {
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
  res.json({
    "status": 200,
    "message": "Outgoing call qeued successfully",
    "data": {
      id: 2345554
    }
  });
});

// https://go.votomobile.org/apidoc/outgoing_calls.html#list-details-of-an-outgoing-call
router.get('/outgoing_calls/:id', function(req, res) {
  res.json({
    "status": 200,
    "code": 1000,
    "data": {
      "outgoing_call": {
        "id": "236900",
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

app.use('/api/v1', router);
app.listen(port);

console.log('Mock API server running on ' + port);
