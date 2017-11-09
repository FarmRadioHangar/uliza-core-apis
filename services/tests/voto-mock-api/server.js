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

app.use('/api/v1', router);
app.listen(port);

console.log('Mock API server running on ' + port);
