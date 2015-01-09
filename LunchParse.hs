
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Functor
import qualified Data.Text as T

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

mensaURL = "http://www.studentenwerk-berlin.de/speiseplan/rss/hu_nord/tag/lang/0000000000000000000000000"

main :: IO ()
main = do
    feed <- runX $ readFeed [ withCurl [] ] mensaURL
    mapM_ maybePrint feed
  where
    maybePrint str =
      let stripped = T.strip $ T.pack str
      in if (not $ T.null stripped) then putStrLn $ T.unpack stripped
         else return ()
    --if rc >= c_err
    --  then exitWith (ExitFailure (0-1))
    --  else exitWith ExitSuccess

readFeed :: SysConfigList -> String -> IOSArrow b String
readFeed cfg src
    = configSysVars cfg
      >>> readDocument [] src
      >>> feedTitle
      <+> feedInner

feedTitle :: IOSArrow XmlTree String
feedTitle = deep ( isElem >>> hasName "title" /> getText)

feedInner :: IOSArrow XmlTree String
feedInner
    = (deep ( isElem >>> hasName "description" /> getText ))
      >>>
      readFromString [withParseHTML yes]
      >>>
      (deep ( isElem >>> hasClass "mensa_day_speise_name" /> getText))

hasClass :: String -> IOSArrow XmlTree XmlTree
hasClass cls = hasAttrValue "class" chk
  where
    chk s = cls `elem` (words s)

