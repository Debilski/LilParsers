
import           Data.Functor
import           Data.List
import qualified Data.Text             as T
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.XML.HXT.Core
import           Text.XML.HXT.Curl

mensaURL = "http://www.studentenwerk-berlin.de/speiseplan/rss/hu_nord/tag/lang/0000000000000000000000000"

main :: IO ()
main = do
    feed <- runX $ readFeed [ withCurl [] ] mensaURL
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
      (deep ( isElem >>> hasClass "mensa_day_speise_name" >>> ((getChildren >>> getText >>> nonEmpty) &&& getFoodAttrs) >>> (arr mkLine)))
  where
    getFoodAttrs = deep (isElem
                   >>> hasName "a"
                   >>> hasClass "zusatz"
                   >>> getAttrValue "title")
                   >. intercalate ", "
    nonEmpty = isA (\s -> (not . null) . strip $ s)
    mkLine :: (String, String) -> String
    mkLine (food, attrs) = (strip food) ++ " (" ++ attrs ++ ")"

hasClass :: String -> IOSArrow XmlTree XmlTree
hasClass cls = hasAttrValue "class" chk
  where
    chk s = cls `elem` (words s)

strip :: String -> String
strip = T.unpack . T.strip . T.pack
