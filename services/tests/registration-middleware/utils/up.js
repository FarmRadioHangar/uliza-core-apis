var Docker = require('simple-dockerode');
var fs     = require('fs');
var path   = require('path');
var stream = require('stream');
var tar    = require('tar-fs');
var term   = require('terminal-kit').terminal ;  
var util   = require('util');

var docker = new Docker();

function createContainer(config) {
  return function() {
    console.log('Create ' + config + ' container');
    return docker.createContainer(require('../config/' + config + '.json'));
  }
}

function imageTags(images) {
  return [].concat.apply([], images.map(function(image) { 
    return image.RepoTags; 
  }));
}

function buildImage(tag, path) {
  return function() {
    return new Promise(function(resolve, reject) {
      return docker.listImages()
      .then(function(images) {
        var cached = imageTags(images), latest = tag + ':latest';
        if (-1 !== cached.indexOf(latest)) {
          console.log('Image found: ' + latest);
          resolve();
        } else {
          var tarStream = tar.pack(path);
          var EchoStream = function() { 
            stream.Writable.call(this); 
          };
          util.inherits(EchoStream, stream.Writable); 
          EchoStream.prototype._write = function(chunk, encoding, done) { 
            var obj;
            try {
              obj = JSON.parse(chunk.toString())
            } catch (e) {
              return done();
            }
            if (obj && obj.stream) {
              process.stdout.write(obj.stream);
            }
            done();
          }
          docker.buildImage(tarStream, {t: tag}, function(error, output) {
            if (error) {
              return reject(error);
            }
            var writeStream = new EchoStream(); 
            output.pipe(writeStream);
            output.on('end', resolve);
          });
        }
      });
    });
  }
}

function acquireBaseImages() {
  return docker.listImages()
  .then(function(images) {
    var cached = imageTags(images);
    return Promise.all(['python:2.7.12', 'haskell:8', 'mysql:5.7']
    .filter(function(name) { 
      var found = -1 !== cached.indexOf(name)
      if (found) {
        console.log('Image ' + name + ' found.');
      }
      return !found; 
    })
    .map(function(name) { 
      return pullImage(name); 
    }));
  });
}

function pullImage(image) {
  return new Promise(function(resolve, reject) {
    console.log('Pulling image ' + image);
    term.saveCursor() ;  
    var progress = {
      status: '',
      info: {}
    };
    docker.pull(image, function(err, stream) {
      if (err) {
        return reject(err);
      }
      docker.modem.followProgress(stream, onFinished, onProgress);
      function onFinished(err, output) {
        if (err) {
          return reject(err);
        }
        resolve(output);
      }
      function onProgress(event) {
        if (event.id) {
          term.restoreCursor() ;  
          term(progress.status);
          progress.info[event.id] = event;
          for (key in progress.info) {
            var data = progress.info[key];
            term(data.status);
            term(' ');
            term(data.progress);
            term.eraseLineAfter();
            term('\n');
          }
        } else {
          progress.status = event.status;
        }
      }
    });
  });
}

function startContainer(name, opts) {
  return function() {
    console.log('Starting container ' + name);
    return docker.getContainer(name).start();
  }
}

function ping(cmd, msg) {
  return new Promise(function(resolve, reject) {
    var options = {stdout: true, stderr: true};
    var retry = function() {
      docker.getContainer('api').exec(cmd, options, function(err, results) {
        if (err) {
          return reject(err);
        }
        process.stdout.write('.');
        if (results.inspect.ExitCode) {
          retry();
        } else {
          console.log('\n' + msg);
          resolve();
        }
      });
    }
    retry();
  });
}

function pingApi() {
  return ping(['curl', 'http://0.0.0.0:8000/api/v1'], 
    'Django server is running.');
}

function pingMysql () {
  return ping(['mysqladmin', 'ping', '-hdatabase', '-uroot', '-proot'], 
    'Database server is accepting connections.');
}

function runExec(container, opts) {
  var opts = ('object' === typeof(opts)) ? opts : {
    Cmd: Array.from(arguments).slice(1), 
    AttachStdin: true, 
    AttachStdout: true,
    tty: true
  };
  return new Promise(function(resolve, reject) {
    container.execRaw(opts, function(err, exec) {
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
        stream.on('end', resolve);
      });
    });
  });
}

function runMigrations() {
  return runExec(docker.getContainer('api'), 'python', 'manage.py', 'migrate'); 
}

function runServer() {
  return runExec(docker.getContainer('api'), {
    Cmd: ['python', 'manage.py', 'runserver', '0.0.0.0:8000'] 
  });
}

function up() {
  return Promise.resolve()
  .then(acquireBaseImages)
  .then(buildImage('django_api', '../../../django-api/'))
  .then(buildImage('registration_middleware', '../../registration-middleware/'))
  .then(createContainer('mysql'))
  .then(createContainer('api'))
  .then(createContainer('middleware'))
  .then(startContainer('database'))
  .then(startContainer('api'))
  // Wait for MySQL to accept connections
  .then(pingMysql)                     
  .then(startContainer('middleware' ))
  .then(runMigrations)
  .then(runServer)
  .then(pingApi) 
  .catch(console.error);
}

module.exports = up;
