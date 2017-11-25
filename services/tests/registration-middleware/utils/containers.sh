#!/bin/bash

docker rm --force $(docker ps -aq --filter "name=ulizatests_*") 2> /dev/null

docker run \
  -d \
  -e "MYSQL_DATABASE=api_core" \
  -e "MYSQL_ROOT_PASSWORD=root" \
  -p"0:3306" \
  --name ulizatests_db \
  mysql:5.7

until mysqladmin ping \
  -uroot \
  -proot \
  -h0.0.0.0 \
  -P$(docker ps \
        --format "{{.Ports}}" \
        --filter "name=ulizatests_db" | sed -n 's/[^:]*:\([0-9]*\).*/\1/p') \
        2> /dev/null ; do echo -n "." ; sleep 3 ; done

docker run \
  -d \
  -p "0:8089" \
  --name ulizatests_voto \
  farmradio/voto_mock_api

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

docker exec -it ulizatests_api python manage.py migrate

function get_port () {
  docker ps \
      --format "{{.Ports}}" \
      --filter "name=$1" | sed -n 's/[^:]*:\([0-9]*\).*/\1/p' 
}

OUT="\
REG_SERVICE_URL=http://0.0.0.0:$(get_port ulizatests_middleware)\n\
DB_HOST=0.0.0.0\n\
DB_PORT=$(get_port ulizatests_db)\n\
VOTO_API_URL=http://0.0.0.0:$(get_port ulizatests_voto)/api/v1\n\
VOTO_API_KEY=XXX_TEST_KEY_XXX"

echo -e "---------------------------------------------------\
 \n$OUT\n---------------------------------------------------\n"

read -r -p "Write above contents to .env? [y/N] " response
if [[ "$response" =~ ^([yY][eE][sS]|[yY])+$ ]]
then
    echo -e $OUT > .env
fi

docker ps
