#!/bin/bash

query="SET FOREIGN_KEY_CHECKS=0; \
       TRUNCATE TABLE uliza_participant_registration_status_log; \
       TRUNCATE TABLE uliza_voto_webhook_log; \
       TRUNCATE TABLE uliza_participants; \
       TRUNCATE TABLE uliza_registration_calls; \
       TRUNCATE TABLE uliza_voto_survey_registration_tree; \
       TRUNCATE TABLE eav_attribute; \
       TRUNCATE TABLE eav_enumgroup; \
       TRUNCATE TABLE eav_enumgroup_enums; \
       TRUNCATE TABLE eav_enumvalue; \
       TRUNCATE TABLE eav_value; \
       SET FOREIGN_KEY_CHECKS=1; \
       INSERT INTO uliza_voto_survey_registration_tree (voto_survey_id, voto_tree_id) VALUES (89324, 19278);"

docker exec -it database mysql -uroot -proot -e "$query" api_core
