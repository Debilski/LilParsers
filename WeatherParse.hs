
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Functor
import Data.List
import qualified Data.Text as T

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

weatherURL loc = "http://weather.yahooapis.com/forecastrss?u=c&w=" ++ loc
berlin = "638242"

main :: IO ()
main = do
    feed <- runX $ readFeed [ withCurl [] ] (weatherURL berlin)
    mapM_ maybePrint feed
  where
    maybePrint str = if (not . null . strip $ str) then putStrLn . strip $ str
         else return ()
    --if rc >= c_err
    --  then exitWith (ExitFailure (0-1))
    --  else exitWith ExitSuccess

readFeed :: SysConfigList -> String -> IOSArrow b String
readFeed cfg src
    = configSysVars cfg
      >>> readDocument [] src
      >>> feedCondition
      <+> feedWind
      <+> feedSunset

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

strip :: String -> String
strip = T.unpack . T.strip . T.pack
