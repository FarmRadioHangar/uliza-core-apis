require('dotenv').config();

var fs     = require('fs');
var Docker = require('simple-dockerode');
var assert = require('assert');
var chai   = require('chai');
var mocha  = require('mocha');
var path   = require('path');
var stream = require('stream');
var util   = require('util');
var targz  = require('tar.gz');

chai.should();
chai.use(require('chai-things'));
chai.use(require('chai-also'));
chai.use(require('chai-http'));

var docker = new Docker();

function createDatabaseContainer() {
  var options = {
    'name': 'database',
    'Image': 'mysql',
    'ExposedPorts': { '3306': {} },
    'Env': [ 
      'MYSQL_DATABASE=api_core',
      'MYSQL_ROOT_PASSWORD=root' 
    ],
    'AttachStdin': false,
    'AttachStdout': true,
    'AttachStderr': true,
    'Tty': true
  };
  return docker.createContainer(options);
}

function createApiContainer() {
  var options = {
    'name': 'api',
    'Image': 'django_api',
    'Cmd': [ 'python', 'manage.py', 'runserver', '0.0.0.0:8000' ],
    'HostConfig': {
      'Links': ['database']
    },
    'Env': [
      'DEBUG="true"',
      'DATABASE_NAME=api_core',
      'DATABASE_USER=root',
      'DATABASE_PASSWORD=root',
      'DATABASE_SERVICE_HOST=database'
    ]
  };
  return docker.createContainer(options);
}

var buildArchive = './.build/django_api.tar.gz';

function createTarArchive() {
  if (fs.existsSync(buildArchive)) {
    console.log('Archive found.');
    return Promise.resolve();
  } else {
    console.log('Creating django-api tar archive.');
    return targz({}, {fromBase: true}).compress('../../../django-api/', buildArchive); 
  }
}

function buildApiImage() {
  return new Promise(function(resolve, reject) {
    var EchoStream = function() { 
      stream.Writable.call(this); 
    };
    util.inherits(EchoStream, stream.Writable); 
    EchoStream.prototype._write = function(chunk, encoding, done) { 
      process.stdout.write(JSON.parse(chunk.toString()).stream);
      done();
    }
    docker.buildImage(buildArchive, {t: 'django_api'}, function(err, stream) {
     if (err) {
        reject(err);
      }
      var writeStream = new EchoStream(); 
      stream.pipe(writeStream, { 
        end: true 
      });
      stream.on('end', resolve);
    });
  });
}

describe('123', function() {

  this.timeout(4000000);

  var self = this;
  self._containers = {};

  var saveContainer = function(name) {
    return function(container) { 
      self._containers[name] = container; 
    }
  };

  var startContainer = function(name) {
    return function() {
      return self._containers[name].start(); 
    }
  };

  var stopContainer = function(name) { 
    return function() {
      return self._containers[name].stop();
    }
  };

  var removeContainer = function(name) { 
    return function() {
      return self._containers[name].remove();
    }
  };

  var makeMigrations = function() { 
    return exec(['python', 'manage.py', 'makemigrations']); 
  };

  var exec = function(cmd, opts) {
    var defaults = {live: true, stdout: true, stderr: true};
    return new Promise(function(resolve, reject) {
      self._containers.api.exec(cmd, opts || defaults, function(err, streamLink) {
        if (err) {
          return reject(err);
        }
        var stream = streamLink(process.stdout, process.stderr);
        stream.on('end', resolve);
      });
    });
  };

  var pingMysql = function() {
    return new Promise(function(resolve, reject) {
      var options = {stdout: true, stderr: true};
      var cmd = ['mysqladmin', 'ping', '-hdatabase', '-uroot', '-proot'];
      var retry = function() {
        self._containers.api.exec(cmd, options, 
        function(err, results) {
          if (err) {
            return reject(err);
          }
          process.stdout.write('.');
          if (results.inspect.ExitCode) {
            retry();
          } else {
            console.log('.');
            resolve();
          }
        });
      }
      retry();
    });
  };

  before(function() { 
    return createTarArchive()
    .then(buildApiImage)
    .then(createDatabaseContainer)
    .then(saveContainer('db'))
    .then(createApiContainer)
    .then(saveContainer('api'))
    .then(startContainer('db'))
    .then(startContainer('api'))
    .then(pingMysql)                     // Wait for MySQL to accept connections
    .then(makeMigrations)
    .catch(function(err) { 
      console.error(err); 
    });
  });

  after(function() { 
    return stopContainer('db')()
    .then(removeContainer('db'))
    .then(stopContainer('api'))
    .then(removeContainer('api'))
    .catch(function(err) { 
      console.error(err); 
    });
  });

  beforeEach(function() { 
    return exec(['python', 'manage.py', 'migrate']);
  });

  afterEach(function() { 
    return exec(['python', 'manage.py', 'migrate', 'uliza', 'zero']);
  });

  it('should be ok', function() {});

  it('should be ok again', function() {});

});
