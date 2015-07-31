
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module GangliaParse
  ( GangliaResult(..)
  , ClusterData(..)
  , MetricData(..)
  , GangliaMetric
  , GangliaCluster
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Map
import GHC.Generics

data GangliaResult a = GangliaResult {
    status :: String
  , message :: a
} deriving (Show, Generic)

data ClusterData = ClusterData {
    clusters :: Map String [String]
  , hosts :: Map String (Map String [String])
  } deriving (Show, Generic)

data MetricData = MetricData {
    metric_value :: String
  , units :: String
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (GangliaResult a)
instance ToJSON a => ToJSON (GangliaResult a)

instance FromJSON ClusterData
instance ToJSON ClusterData

instance FromJSON MetricData
instance ToJSON MetricData

type GangliaMetric = GangliaResult MetricData
type GangliaCluster = GangliaResult ClusterData

