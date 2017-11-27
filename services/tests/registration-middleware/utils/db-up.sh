#!/bin/bash
docker run --name database -p3306:3306 -e MYSQL_DATABASE=api_core -e MYSQL_ROOT_PASSWORD=root -d mysql:5.7
