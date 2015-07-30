
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module GangliaParse
  ( GangliaResult(..)
  , GangliaData(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Map
import GHC.Generics

data GangliaResult = GangliaResult {
    status :: String
  , message :: GangliaData
} deriving (Show, Generic)

data GangliaData =
  ClusterData {
    clusters :: Map String [String]
  , hosts :: Map String (Map String [String])
  } |
  MetricData {
    metric_value :: String
  , units :: String
  } deriving (Show, Generic)

instance FromJSON GangliaResult
instance ToJSON GangliaResult

instance FromJSON GangliaData
  where
    parseJSON (Object o) =
      MetricData <$> (o .: "metric_value") <*> (o .: "units")
      <|>
      ClusterData <$> (o .: "clusters") <*> (o .: "hosts")

instance ToJSON GangliaData

