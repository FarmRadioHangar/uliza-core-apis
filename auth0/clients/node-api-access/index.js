var Auth0Strategy = require('passport-auth0');
var bodyParser    = require('body-parser');
var cookieParser  = require('cookie-parser');
var debug         = require('debug')('node-api-access-demo:server');
var dotenv        = require('dotenv');
var express       = require('express');
var http          = require('http');
var passport      = require('passport');
var path          = require('path');
var request       = require('request-promise');
var session       = require('express-session');

dotenv.load();

var strategy = new Auth0Strategy({
  domain: process.env.AUTH0_DOMAIN,
  clientID: process.env.AUTH0_CLIENT_ID,
  clientSecret: process.env.AUTH0_CLIENT_SECRET,
  callbackURL: process.env.AUTH0_CALLBACK_URL || 'http://localhost:3001/callback'
}, function(accessToken, refreshToken, extraParams, profile, done) {
  return done(null, {
    profile: profile,
    accessToken: accessToken,
    refreshToken: refreshToken,
    extraParams: extraParams
  });
});

passport.use(strategy);

passport.serializeUser(function(user, done) {
  done(null, user);
});

passport.deserializeUser(function(user, done) {
  done(null, user);
});

var app = express();

app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());

app.use(session({
  secret: 'VtaskhvaXhg3wC0btTb1778XibUSBDBT',
  resave: true,
  saveUninitialized: true
}));

app.use(passport.initialize());
app.use(passport.session());

app.get('/', function(req, res) {
  res.render('index', {
    loggedIn: !!req.user
  });
});

app.get('/login', function(req, res) {
  res.render('login', { 
    env: {
      AUTH0_CLIENT_ID: process.env.AUTH0_CLIENT_ID,
      AUTH0_DOMAIN: process.env.AUTH0_DOMAIN,
      AUTH0_CALLBACK_URL: process.env.AUTH0_CALLBACK_URL || 'http://localhost:3001/callback'
    }
  });
});

app.get('/logout', function(req, res) {
  req.logout();
  res.redirect('/');
});

app.get('/error', function(req, res) {
  res.send('There was an error.');
});

app.get('/callback', 
  passport.authenticate('auth0', { 
    audience: 'https://dev.farmradio.fm/api/',
    failureRedirect: '/error' 
  }), function(req, res) {
    res.redirect('/');
  }
);

app.get('/secret', function(req, res) {
  var options = {
    uri: 'http://localhost:8080/protected',
    json: true
  };
  if (req.user) {
    options.headers = { authorization: 'Bearer ' + req.user.extraParams.access_token };
  }
  request.get(options)
    .then(function(json) {
      res.render('secret', {
        message: json.message
      });
    })
    .catch(function(err) {
      if ('RequestError' === err.name) {
        return res.send('Something went wrong. Make sure that the API server is running and listening on port 8080.');
      } else if (err.response) {
        if (401 === err.response.statusCode) {
          return res.render('unauthorized');
        }
      } 
      res.send('Something went wrong.');
      throw err;
    });
});

var server = http.createServer(app);

server.on('error', function(error) {
  if (error.syscall !== 'listen') {
    throw error;
  }
  var bind = typeof port === 'string' ? 'Pipe ' + port : 'Port ' + port;
  switch (error.code) {
    case 'EACCES':
      console.error(bind + ' requires elevated privileges');
      process.exit(1);
      break;
    case 'EADDRINUSE':
      console.error(bind + ' is already in use');
      process.exit(1);
      break;
    default:
      throw error;
  }
});

server.on('listening', function() {
  var addr = server.address();
  var bind = typeof addr === 'string' ? 'pipe ' + addr : 'port ' + addr.port;
  debug('Listening on ' + bind);
  console.log('Listening on ' + bind);
});

server.listen(3001);
