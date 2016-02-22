
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import           Data.Functor
import           Data.List
import           Data.Map
import           Data.Text             hiding (replicate)
import qualified Data.Text             as T
import           Data.Text.IO
import           Debug.Trace
import           Network.HTTP.Conduit  (simpleHttp)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Printf

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

sizeof_fmt :: String -> Int -> String
sizeof_fmt suffix 0 = printf "0 %s" suffix
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
    mem_buffers <- readMetric' "mem_buffers" srv
    mem_cached <- readMetric' "mem_cached" srv
    mem_shared <- readMetric' "mem_shared" srv
    mem_total <- readMetric' "mem_total" srv
    swap_free <- readMetric' "swap_free" srv
    swap_total <- readMetric' "swap_total" srv
    let mem_used = Data.List.foldl1 (liftA2 (-)) [mem_total, mem_free, mem_cached]
    let swap_used = (-) <$> swap_total <*> swap_free
    let res = line <$> mem_used <*> mem_total <*> swap_used <*> swap_total
    case res of
      Left err -> Data.Text.IO.putStrLn $ T.pack $ printf "%s: No data available." (T.unpack srv)
      Right ps -> Data.Text.IO.putStrLn ps
  where
    readMetric' :: Text -> Text -> IO (Either String Int)
    readMetric' m s = (fmap . fmap) (read . metric_value . message) (readMetric m s)

    line :: Int -> Int -> Int -> Int -> Text
    line mem_used mem_total swap_used swap_total = T.pack $ printf "%s: %s Memory %s used (of %s) with %s of swap (total %s)." (T.unpack srv) (bar 30 mem_used mem_total swap_used swap_total) (human mem_used) (human mem_total) (human swap_used) (human swap_total)
    human = sizeof_fmt "B"
    val :: GangliaMetric -> String
    val = metric_value . message

    bar :: Int -> Int -> Int -> Int -> Int -> String
    bar width mem_used mem_total swap_used swap_total =
      let
        mt = (width * mem_total) `div` (mem_total + swap_total)
        mf = (width * (mem_total - mem_used)) `div` (mem_total + swap_total)
        st = (width * swap_total) `div` (mem_total + swap_total)
        sf = (width * (swap_total - swap_used)) `div` (mem_total + swap_total)
        mu = mt - mf
        su = st - sf
      in "[" ++ (replicate mu '=')  ++ (replicate mf ' ') ++ "|" ++ (replicate su '=') ++ (replicate sf ' ') ++ "]"

readMetric :: Text -> Text -> IO (Either String GangliaMetric)
readMetric metric srv = do
    -- Get JSON data and decode it
    let json = getJSON metric srv
    json >>= B.hPutStr stderr
    System.IO.hPutStrLn stderr ""
    d <- (eitherDecode <$> json) :: IO (Either String GangliaMetric)
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
