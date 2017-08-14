{-# LANGUAGE OverloadedStrings #-}
module FarmRadio.Voto.Client where

import Data.Aeson
import Data.Maybe   ( fromJust )
import Data.Text

votoSubscriber :: Int -> IO Value
votoSubscriber subscriberId = return test_subscriber_1

test_subscriber_1 :: Value 
test_subscriber_1 = fromJust $ decode "{ \"status\": 200, \"code\": 1000, \"data\": { \"subscriber\": { \"id\": \"373751\", \"receive_sms\": \"1\", \"receive_voice\": \"1\", \"receive_data\": \"0\", \"receive_ussd\": \"0\", \"phone\": \"255786082881\", \"active\": \"1\", \"start_date\": \"2014-03-12\", \"language_id\": \"200715\", \"is_test_subscriber\": \"1\", \"group_ids\": \"200605, 201212, 222874\", \"name\": \"Bart Sullivan\", \"location\": \"Arusha\", \"comments\": \"For the switch board\", \"properties\": { \"name\": \"Bart Sullivan\", \"location\": \"Arusha\", \"comments\": \"For the switch board\", \"gender\": \"Female\", \"age_group\": \"35-50\", \"occupation\": \"Farmer\", \"zone\": null, \"region\": null, \"district\": null, \"registered\": \"true\", \"registration\": null, \"registration_status\": null, \"age\": null, \"yes\": null, \"no\": null, \"wilaya\": null, \"kijiji\": null, \"register\": null } } }, \"message\": \"Subscriber details fetched successfully\", \"more_info\": \"\", \"pagination\": null, \"url\": \"https://go.votomobile.org/api/v1/subscribers/373751?api_key=ce919f9c9f6f6dc9a17b6adb6\" }"
