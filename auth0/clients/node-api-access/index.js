var express       = require('express');
var http          = require('http');
var cookieParser  = require('cookie-parser');
var bodyParser    = require('body-parser');
var session       = require('express-session');
var dotenv        = require('dotenv');
var passport      = require('passport');
var Auth0Strategy = require('passport-auth0');

