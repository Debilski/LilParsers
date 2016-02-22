
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import           Data.Functor
import           Data.List
import           Data.Map
import           Data.Text
import qualified Data.Text             as T
import           GHC.Generics
import           Network.HTTP.Conduit  (simpleHttp)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           GangliaParse


monitoringURL :: String -> String -> String
monitoringURL metric host = "http://monitoring.itb.pri/ganglia/api/metrics.php?metric_name=" ++ metric ++ "&host=" ++ host
hostURL = "http://monitoring.itb.pri/ganglia/api/host.php?action=list"

-- Read the remote copy of the JSON file.
getJSON :: Text -> Text -> IO B.ByteString
getJSON metric srv = simpleHttp (monitoringURL (T.unpack metric) (T.unpack srv))

main :: IO ()
main = do
    let hosts = simpleHttp hostURL
    d <- (eitherDecode <$> hosts) :: IO (Either String GangliaCluster)
    case d of
      Left err -> System.IO.putStrLn err
      Right res -> mapM_ readServer (getServers res)
  where
    getServers res = T.pack <$> (clusters . message $ res) ! "Compute Cluster"

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
    val :: GangliaMetric -> String
    val = metric_value . message

readMetric :: Text -> Text -> IO (Either String GangliaMetric)
readMetric metric srv = do
    -- Get JSON data and decode it
    d <- (eitherDecode <$> getJSON metric srv) :: IO (Either String GangliaMetric)
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
