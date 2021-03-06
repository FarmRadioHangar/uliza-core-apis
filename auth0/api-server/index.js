var express = require('express');
var jwt = require('express-jwt');
var jwks = require('jwks-rsa');

var app = express();
var port = 8080;

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

app.use(function(err, req, res, next) {
  if (err.name === 'UnauthorizedError') {
    res.status(401).send();
  } else {
    next(err);
  }
});

app.get('/protected', function(req, res) {
  res.json({
    message: 'This API is a teapot.'
  });
});

app.listen(port, null, function() {
  console.log('Listening on port ' + port);
});
