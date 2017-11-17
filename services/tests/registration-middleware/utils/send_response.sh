#!/bin/bash
curl \
  -XPOST \
  http://13.81.253.165:3034/responses \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d 'question_id=127375&survey_id=89324&voto_id=44&response_type=1&content_type=1&poll_id=213&delivery_log_id=832&choice_id=1&subscriber_id=232&subscriber_phone=%2B256784224203&question_title=Who%20was%20the%20first%20man%20to%20set%20foot%20on%20the%20moon%3F&choice_name=Neil%20Armstrong&date_received=2017-11-17T12%3A13%3A51Z'
