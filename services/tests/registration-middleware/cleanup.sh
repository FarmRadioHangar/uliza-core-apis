#!/bin/bash

# Remove exited containers
docker ps --filter status=dead --filter status=exited -aq | xargs -r docker rm -v
    
# Remove unused images
docker images --no-trunc | grep '<none>' | awk '{ print $3 }' | xargs -r docker rmi

# Remove unused volumes
docker volume ls -qf dangling=true | xargs -r docker volume rm
