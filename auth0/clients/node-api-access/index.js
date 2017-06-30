var Auth0Strategy = require('passport-auth0');
var bodyParser    = require('body-parser');
var cookieParser  = require('cookie-parser');
var debug         = require('debug')('node-api-access-demo:server');
var dotenv        = require('dotenv');
var express       = require('express');
var http          = require('http');
var passport      = require('passport');
var session       = require('express-session');

dotenv.load();

var strategy = new Auth0Strategy({
  domain:       process.env.AUTH0_DOMAIN,
  clientID:     process.env.AUTH0_CLIENT_ID,
  clientSecret: process.env.AUTH0_CLIENT_SECRET,
  callbackURL:  process.env.AUTH0_CALLBACK_URL || 'http://localhost:3001/callback'
}, function(accessToken, refreshToken, extraParams, profile, done) {
  return done(null, profile);
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

app.use('/', function(req, res) {
  res.send('200 OK');
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

app.set('port', 3001);

server.listen(3001);
