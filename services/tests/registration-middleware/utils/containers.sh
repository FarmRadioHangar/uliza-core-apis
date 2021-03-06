#!/bin/bash

# Check if the 'live' option was provided
LIVE_API=$([ "$1" = "live" ] && echo true || echo false)

# Remove any containers left over from earlier sessions
docker rm --force $(docker ps -aq --filter "name=ulizatests*") 2> /dev/null

# Create and run mysql container
docker run \
  -d \
  -e "MYSQL_DATABASE=uliza_core" \
  -e "MYSQL_ROOT_PASSWORD=root" \
  -p "0:3306" \
  --name ulizatestsdb \
  mysql:5.7

# Wait for database server to accept connections
echo "Waiting for mysql to accept connections"
until mysqladmin ping \
  -uroot \
  -proot \
  -h0.0.0.0 \
  -P$(docker ps \
        --format "{{.Ports}}" \
        --filter "name=ulizatestsdb" | sed -n 's/[^:]*:\([0-9]*\).*/\1/p') \
        2> /dev/null ; do echo -n "." ; sleep 3 ; done


# Create and run mock VOTO API container
if [ "$LIVE_API" != true ] ; then
    docker run \
      -d \
      -p "0:8089" \
      --name ulizatestsvoto \
      farmradio/voto_mock_api
fi

# Create and run Uliza API container
docker run \
  -d \
  -e "DEBUG=true" \
  -e "DB_ENGINE=django.db.backends.mysql" \
  -e "DB_NAME=uliza_core" \
  -e "DB_USER=root" \
  -e "DB_PASSWORD=root" \
  -e "DB_SERVICE_HOST=ulizatestsdb" \
  -e "DB_SERVICE_PORT=3306" \
  -p "0:8000" \
  --link ulizatestsdb \
  --name ulizatestsapi \
  farmradio/uliza_api

# Create and run registration service container
args=(
  -d 
  -p "0:3034" 
  -e "PORT=3034" 
  -e "LOG_LEVEL=DEBUG" 
  -e "ULIZA_API_URL=http://ulizatestsapi:8000/api/v1" 
  -e "CALL_SCHEDULE_OFFSET=600" 
  -e "MIN_RESCHEDULE_DELAY=172800" 
  --link ulizatestsapi 
  --name ulizatestsmiddleware 
)

if [ "$LIVE_API" = true ] ; then
args+=(
  -e "VOTO_API_URL=https://go.votomobile.org/api/v1" 
  -e "VOTO_API_KEY=$2"
)
else
args+=(
  -e "VOTO_API_URL=http://ulizatestsvoto:8089/api/v1" 
  --link ulizatestsvoto 
)
fi

docker run "${args[@]}" farmradio/registration_service

# Apply database migrations
docker exec -it ulizatestsdb mysql -uroot -proot \
  -e"USE uliza_core; DROP TABLE IF EXISTS django_migrations;"
docker exec -it ulizatestsapi ./django.sh makemigrations 
docker exec -it ulizatestsapi ./django.sh migrate

function get_port () {
  docker ps \
      --format "{{.Ports}}" \
      --filter "name=$1" | sed -n 's/[^:]*:\([0-9]*\).*/\1/p' 
}

if [ "$LIVE_API" != true ] ; then

# Collect ENV variables for Mocha tests
OUT="\
REG_SERVICE_URL=http://0.0.0.0:$(get_port ulizatestsmiddleware)\n\
DB_HOST=0.0.0.0\n\
DB_PORT=$(get_port ulizatestsdb)\n\
ULIZA_API_URL=http://0.0.0.0:$(get_port ulizatestsapi)/api/v1\n\
VOTO_API_URL=http://0.0.0.0:$(get_port ulizatestsvoto)/api/v1\n\
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

fi # if [ "$LIVE_API" != true ] 

# List running containers
docker ps

# API
URL=$(docker ps \
  --filter "name=ulizatestsapi" \
  --format "{{.Ports}}" | sed -n 's/\([0-9.:]*\).*/\1/p')
echo -e "---------------------------------------------------\
  \nUliza API url: http://$URL/api/v1"
