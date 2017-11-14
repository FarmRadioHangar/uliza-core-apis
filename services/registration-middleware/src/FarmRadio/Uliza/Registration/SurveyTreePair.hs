{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FarmRadio.Uliza.Registration.SurveyTreePair
  ( SurveyTreePair(..)
  ) where

import Data.Aeson
import Data.Text

data SurveyTreePair = SurveyTreePair
    { votoSurveyId  :: !Int
    , votoTreeId    :: !Int }
  deriving (Show, Eq)

instance FromJSON SurveyTreePair where
  parseJSON = withObject "SurveyTreePair" $ \v -> SurveyTreePair
    <$> v .: "voto_survey_id"
    <*> v .: "voto_tree_id"
