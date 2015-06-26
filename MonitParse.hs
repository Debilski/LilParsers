
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Functor
import Data.List
import qualified Data.Text as T



monitoringURL metric host = "http://monitoring.itb.pri/ganglia/api/metrics.php?metric_name=" ++ metric ++ "&host=" ++ host

data Metric = Metric {
    status :: String
  , message :: MetricData
} deriving (Show,Generic)

data MetricData = MetricData {
    metric_value :: String
  , units :: String
} deriving (Show, Generic)

instance FromJSON Metric
instance ToJSON Metric

instance FromJSON MetricData
instance ToJSON MetricData

-- Read the remote copy of the JSON file.
getJSON :: Text -> Text -> IO B.ByteString
getJSON metric srv = simpleHttp (monitoringURL (T.unpack metric) (T.unpack srv))

servers = fmap ((flip append) ".itb.pri") ("compute1" : "compute2" : "compute3" : [])

main :: IO ()
main = mapM_ readServer servers

readServer :: Text -> IO ()
readServer srv = do
    cpu_num <- readMetric "cpu_num" srv
    load_one <- readMetric "load_one" srv
    let res = line <$> (val <$> cpu_num) <*> (val <$> load_one)
    case res of
      Left err -> putStrLn $ (T.unpack srv) ++ ": No data available."
      Right ps -> putStrLn ps
  where
    line cpu load = (T.unpack srv) ++ ": Load " ++ load ++ " per " ++ cpu ++ " CPUs."
    val :: Metric -> String
    val = metric_value . message

readMetric :: Text -> Text -> IO (Either String Metric)
readMetric metric srv = do
    -- Get JSON data and decode it
    d <- (eitherDecode <$> getJSON metric srv) :: IO (Either String Metric)
    return d
    -- If d is Left, the JSON was malformed.
    -- In that case, we report the error.
    -- Otherwise, we perform the operation of
    -- our choice. In this case, just print it.
--    case d of
--      Left err -> putStrLn err
--      Right ps -> print ps

strip :: String -> String
strip = T.unpack . T.strip . T.pack
