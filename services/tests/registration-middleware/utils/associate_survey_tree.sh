#!/bin/bash
curl \
  -XPOST \
  http://13.81.253.165/api/v1/voto_survey_registration_tree \
  -H 'Content-Type: application/json' \
  -d '{"voto_survey_id": 89324, "voto_tree_id": 19278}'
