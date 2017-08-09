#!/bin/bash
set -e

psql postgres://$POSTGRES_USER:$POSTGRES_PASSWORD@localhost/$POSTGRES_DB -v ON_ERROR_STOP=1 <<-EOSQL
  CREATE ROLE www NOLOGIN; 
  CREATE ROLE app NOLOGIN; 
  GRANT www TO postgres; 
  GRANT app TO postgres; 
  CREATE SCHEMA farmradio_api;
  GRANT USAGE ON SCHEMA farmradio_api TO www;
  GRANT USAGE ON SCHEMA farmradio_api TO app;
EOSQL
