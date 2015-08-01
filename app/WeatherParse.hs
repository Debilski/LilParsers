
import           Data.Functor
import           Data.List
import qualified Data.Text             as T
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.XML.HXT.Core
import           Text.XML.HXT.Curl

weatherURL loc = "http://weather.yahooapis.com/forecastrss?u=c&w=" ++ loc
berlin = "638242"

main :: IO ()
main = do
    args <- getArgs
    feed <- runX $ (readFeed args) [ withCurl [] ] (weatherURL berlin)
    mapM_ maybePrint feed
  where
    maybePrint str = if (not . null . strip $ str) then putStrLn . strip $ str
         else return ()
    --if rc >= c_err
    --  then exitWith (ExitFailure (0-1))
    --  else exitWith ExitSuccess

readFeed :: [String] -> SysConfigList -> String -> IOSArrow b String
readFeed args cfg src
    = configSysVars cfg
      >>> readDocument [] src
      >>> if (length args == 0) then
            currentWeather
          else
            forecastWeather (head args)
  where
    currentWeather = feedCondition
      <+> feedWind
      <+> feedSunset
    forecastWeather day = feedForecast day

feedCondition :: IOSArrow XmlTree String
feedCondition = deep ( isElem >>> hasName "yweather:condition" >>> ( getAttrValue "text" &&& getAttrValue "temp" ) >>> (arr mkLine))
  where
    mkLine (cond, temp) = "Currently " ++ cond ++ ", " ++ temp ++ " °C"

feedWind :: IOSArrow XmlTree String
feedWind = deep ( isElem >>> hasName "yweather:wind" >>> ( getAttrValue "speed" &&& getAttrValue "chill" ) >>> (arr mkLine))
  where
    mkLine (speed, temp) = "Wind: " ++ speed ++ "km/h. Feels like " ++ temp ++ " °C"

feedSunset :: IOSArrow XmlTree String
feedSunset = deep ( isElem >>> hasName "yweather:astronomy" >>> ( getAttrValue "sunrise" &&& getAttrValue "sunset" ) >>> (arr mkLine))
  where
    mkLine (rise, set) = "Sunrise: " ++ rise ++ ", Sunset: " ++ set

feedForecast :: String -> IOSArrow XmlTree String
feedForecast day = deep ( isElem >>> hasName "yweather:forecast" >>> ( hasAttrValue "day" (== day) >>> getAttrValue "date" &&& getAttrValue "low" &&& getAttrValue "high" &&& getAttrValue "text") >>> (arr mkLine)) `orElse` (constA $ "No forecast available for " ++ day)
  where
    mkLine (date, (low, (high, text))) = "Forecast for " ++ date ++ ": " ++ text ++ " between " ++ low ++ " °C and " ++ high ++ " °C"


strip :: String -> String
strip = T.unpack . T.strip . T.pack
