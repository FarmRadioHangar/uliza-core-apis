var Docker = require('simple-dockerode');

var docker = new Docker();

function stopContainer(name) { 
  return function() {
    console.log('Stopping container ' + name);
    return docker.getContainer(name).stop();
  }
}

function removeContainer(name) { 
  return function() {
    return docker.getContainer(name).remove();
  }
}

function down() {
  return Promise.resolve()
  .then(stopContainer('middleware'))
  .then(removeContainer('middleware'))
  .then(stopContainer('api'))
  .then(removeContainer('api'))
  .then(stopContainer('database'))
  .then(removeContainer('database'))
  .catch(console.error);
}

module.exports = down;
