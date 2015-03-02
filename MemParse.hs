
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text hiding (replicate)
import Text.Printf
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import Debug.Trace

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Functor
import Data.List
import qualified Data.Text as T
import Data.Text.IO

monitoringURL metric host = "http://monitoring/ganglia/api/metrics.php?metric_name=" ++ metric ++ "&host=" ++ host

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

sizeof_fmt :: String -> Int -> String
sizeof_fmt suffix num = let
    units = ["","Ki","Mi","Gi","Ti","Pi","Ei","Zi"] :: [String]
    base = 1024
    exp = (floor $ logBase (fromIntegral base) (fromIntegral num))
    quot = (fromIntegral num) / ((fromIntegral base) ** (fromIntegral exp)) :: Float
    in case exp of
         x | x < 2 -> printf "%.1f %s%s" quot (units !! (1 + exp)) suffix
         otherwise -> printf "%.2f %s%s" quot (units !! (1 + exp)) suffix


readServer :: Text -> IO ()
readServer srv = do
    mem_free <- readMetric' "mem_free" srv
    mem_total <- readMetric' "mem_total" srv
    swap_free <- readMetric' "swap_free" srv
    swap_total <- readMetric' "swap_total" srv
    let res = line <$> mem_free <*> mem_total <*> swap_free <*> swap_total
    case res of
      Left err -> Data.Text.IO.putStrLn $ T.pack $ printf "%s: No data available." (T.unpack srv)
      Right ps -> Data.Text.IO.putStrLn ps
  where
    readMetric' :: Text -> Text -> IO (Either String Int)
    readMetric' m s = (fmap . fmap) (read . metric_value . message) (readMetric m s)

    line :: Int -> Int -> Int -> Int -> Text
    line mem_free mem_total swap_free swap_total = T.pack $ printf "%s: %s Memory %s free (of %s) with %s of swap (total %s)." (T.unpack srv) (bar 30 mem_free mem_total swap_free swap_total) (human mem_free) (human mem_total) (human swap_free) (human swap_total)
    human = sizeof_fmt "B"
    val :: Metric -> String
    val = metric_value . message

    bar :: Int -> Int -> Int -> Int -> Int -> String
    bar width mem_free mem_total swap_free swap_total =
      let
        mt = (width * mem_total) `div` (mem_total + swap_total)
        mf = (width * mem_free) `div` (mem_total + swap_total)
        st = (width * swap_total) `div` (mem_total + swap_total)
        sf = (width * swap_free) `div` (mem_total + swap_total)
        mu = mt - mf
        su = st - sf
      in "[" ++ (replicate mu '=')  ++ (replicate mf ' ') ++ "|" ++ (replicate su '=') ++ (replicate sf ' ') ++ "]"

readMetric :: Text -> Text -> IO (Either String Metric)
readMetric metric srv = do
    -- Get JSON data and decode it
    let json = getJSON metric srv
    json >>= B.hPutStr stderr
    System.IO.hPutStrLn stderr ""
    d <- (eitherDecode <$> json) :: IO (Either String Metric)
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