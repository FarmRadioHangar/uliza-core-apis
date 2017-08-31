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

function createMiddlewareContainer() {
  console.log('Create middleware container');
  return docker.createContainer({
    'name': 'middleware',
    'Image': 'registration_middleware',
    'ExposedPorts': { '3034': {} },
    'HostConfig': {
      'Links': ['api'],
      'PortBindings': { '3034': [{'HostPort': '3034'}] }
    }
  });
}

function createDatabaseContainer() {
  console.log('Create mysql container');
  return docker.createContainer({
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
  });
}

function createApiContainer() {
  console.log('Create api container');
  return docker.createContainer({
    'name': 'api',
    'Image': 'django_api',
    'Cmd': [ 'python', 'manage.py', 'runserver', '0.0.0.0:8000' ],
    'ExposedPorts': { '8000': {} },
    'HostConfig': {
      'Links': ['database'],
      'PortBindings': { '8000': [{'HostPort': '8000'}] }
    },
    'Env': [
      'DEBUG="true"',
      'DATABASE_NAME=api_core',
      'DATABASE_USER=root',
      'DATABASE_PASSWORD=root',
      'DATABASE_SERVICE_HOST=database'
    ]
  });
}

function createArchive(path, archive) {
  return function() {
    return new Promise(function(resolve, reject) {
      if (fs.existsSync(archive)) {
        console.log('Archive found: ' + archive);
        resolve(archive);
      } else {
        console.log('Creating django-api tar archive: ' + archive);
        targz({}, {fromBase: true}).compress(path, archive).then(function() { 
          resolve(archive); 
        }); 
      }
    });
  }
}

function buildImage(tag) {
  return function(archive) {
    return new Promise(function(resolve, reject) {
      var EchoStream = function() { 
        stream.Writable.call(this); 
      };
      util.inherits(EchoStream, stream.Writable); 
      EchoStream.prototype._write = function(chunk, encoding, done) { 
        process.stdout.write(JSON.parse(chunk.toString()).stream);
        done();
      }
      console.log('Build image ' + tag + ' from archive ' + archive);
      docker.buildImage(archive, {t: tag}, function(err, stream) {
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

  var startContainer = function(name, opts) {
    return function() {
      console.log('Starting container ' + name);
      return self._containers[name].start(opts); 
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

  var runExec = function(container) {
    var cmd = Array.from(arguments).slice(1);
    return new Promise(function(resolve, reject) {
      container.execRaw({
        Cmd: cmd, 
        AttachStdin: true, 
        AttachStdout: true,
        tty: true
      }, function(err, exec) {
        if (err) {
          console.error(err);
          return;
        }
        exec.start({
          hijack: true, 
          stdin: true
        }, function(err, stream) {
          if (err) {
            console.error(err);
            return;
          }
          container.modem.demuxStream(stream, process.stdout, process.stderr);
          process.stdin.pipe(stream);
          bufs = [];
          stream.on('end', resolve);
        });
      });
    });
  };

  var makeMigrations = function() {
    return runExec(self._containers.api, 'python', 'manage.py', 'makemigrations');
  };

  var exec = function() {
    var cmd = Array.from(arguments);
    return new Promise(function(resolve, reject) {
      self._containers.api.exec(cmd, {live: true, stdout: true, stderr: true}, 
      function(err, streamLink) {
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
            console.log('Database server is accepting connections.');
            resolve();
          }
        });
      }
      retry();
    });
  };

  before(function() { 
    return new Promise(function(resolve, reject) { resolve(); })
    .then(createArchive('../../../django-api/', './.build/django_api.tar.gz'))
    .then(buildImage('django_api'))
    .then(createArchive('../../registration-middleware/', './.build/middleware.tar.gz'))
    .then(buildImage('registration_middleware'))
    .then(createDatabaseContainer)
    .then(saveContainer('db'))
    .then(createApiContainer)
    .then(saveContainer('api'))
    .then(createMiddlewareContainer)
    .then(saveContainer('middleware'))
    .then(startContainer('db'))
    .then(startContainer('api'))
    .then(startContainer('middleware' ))
    .then(pingMysql)                     // Wait for MySQL to accept connections
    .then(makeMigrations)
    .catch(console.error);
  });

  after(function() { 
    return stopContainer('db')()
    .then(removeContainer('db'))
    .then(stopContainer('middleware'))
    .then(removeContainer('middleware'))
    .then(stopContainer('api'))
    .then(removeContainer('api'))
    .catch(console.error);
  });

  beforeEach(function() { 
    return exec('python', 'manage.py', 'migrate');
  });

  afterEach(function() { 
    return exec('python', 'manage.py', 'migrate', 'uliza', 'zero');
  });

  it('should be ok', function() {});

  it('should be ok again', function() {});

});
