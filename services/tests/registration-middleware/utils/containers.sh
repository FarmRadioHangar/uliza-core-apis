#!/bin/bash

# Remove containers left over from earlier sessions
docker rm --force $(docker ps -aq --filter "name=ulizatests_*") 2> /dev/null

# Create and run mysql container
docker run \
  -d \
  -e "MYSQL_DATABASE=api_core" \
  -e "MYSQL_ROOT_PASSWORD=root" \
  -p"0:3306" \
  --name ulizatests_db \
  mysql:5.7

# Wait for database server to accept connections
echo "Waiting for mysql to accept connections"
until mysqladmin ping \
  -uroot \
  -proot \
  -h0.0.0.0 \
  -P$(docker ps \
        --format "{{.Ports}}" \
        --filter "name=ulizatests_db" | sed -n 's/[^:]*:\([0-9]*\).*/\1/p') \
        2> /dev/null ; do echo -n "." ; sleep 3 ; done

# Create and run mock VOTO API container
docker run \
  -d \
  -p "0:8089" \
  --name ulizatests_voto \
  farmradio/voto_mock_api

# Create and run Uliza API container
docker run \
  -d \
  -e "DEBUG=true" \
  -e "DB_ENGINE=django.db.backends.mysql" \
  -e "DB_NAME=api_core" \
  -e "DB_USER=root" \
  -e "DB_PASSWORD=root" \
  -e "DB_SERVICE_HOST=ulizatests_db" \
  -e "DB_SERVICE_PORT=3306" \
  -p "0:8000" \
  --link ulizatests_db \
  --name ulizatests_api \
  farmradio/uliza_api

# Create and run registration service container
docker run \
  -d \
  -p "0:3034" \
  -e "PORT=3034" \
  -e "LOG_LEVEL=DEBUG" \
  -e "VOTO_API_URL=http://ulizatest_voto:8089/api/v1" \
  -e "ULIZA_API_URL=http://ulizatests_api:8000/api/v1" \
  -e "CALL_SCHEDULE_OFFSET=600" \
  -e "MIN_RESCHEDULE_DELAY=172800" \
  --link ulizatests_api \
  --link ulizatests_voto \
  --name ulizatests_middleware \
  farmradio/registration_service

# Apply database migrations
docker exec -it ulizatests_api python manage.py migrate

function get_port () {
  docker ps \
      --format "{{.Ports}}" \
      --filter "name=$1" | sed -n 's/[^:]*:\([0-9]*\).*/\1/p' 
}

# Create ENV variables for tests
OUT="\
REG_SERVICE_URL=http://0.0.0.0:$(get_port ulizatests_middleware)\n\
DB_HOST=0.0.0.0\n\
DB_PORT=$(get_port ulizatests_db)\n\
ULIZA_API_URL=http://0.0.0.0:$(get_port ulizatests_api)/api/v1\n\
VOTO_API_URL=http://0.0.0.0:$(get_port ulizatests_voto)/api/v1\n\
VOTO_API_KEY=XXX_TEST_KEY_XXX"

echo -e "---------------------------------------------------\
 \n$OUT\n---------------------------------------------------\n"

# Confirm with user to overwrite .env
read -r -p "Write above contents to .env? [y/N] " response
if [[ "$response" =~ ^([yY][eE][sS]|[yY])+$ ]]
then
    echo -e $OUT > .env
    echo ".env file updated"
fi

# List running containers
docker ps
