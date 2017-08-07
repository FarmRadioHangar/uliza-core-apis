#!/bin/bash
set -e
(cd /home/db && npm install && npm run migrations up)
