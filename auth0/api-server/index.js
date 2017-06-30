var express = require('express');
var jwt = require('express-jwt');
var jwks = require('jwks-rsa');

var app = express();
var port = process.env.PORT || 8080;

var jwtCheck = jwt({
  secret: jwks.expressJwtSecret({
    cache: true,
    rateLimit: true,
    jwksRequestsPerMinute: 5,
    jwksUri: "https://farmradio.eu.auth0.com/.well-known/jwks.json"
  }),
  audience: 'https://dev.farmradio.fm/api/',
  issuer: "https://farmradio.eu.auth0.com/",
  algorithms: ['RS256']
});

app.use(jwtCheck);

app.get('/protected', function(req, res) {
  res.json({
    message: 'This API is a teapot.'
  });
});

console.log('Listening on port ' + port);

app.listen(port);
