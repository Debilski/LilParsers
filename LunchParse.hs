
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
    title <- runX $ applicationTitle [ withCurl [] ] mensaURL
    mapM_ maybePrint title
    menu <- runX $ application [ withCurl [] ] mensaURL
    mapM_ maybePrint menu
  where
    maybePrint str =
      let stripped = T.strip $ T.pack str
      in if (not $ T.null stripped) then putStrLn $ T.unpack stripped
         else return ()
    --if rc >= c_err
    --  then exitWith (ExitFailure (0-1))
    --  else exitWith ExitSuccess


applicationTitle     :: SysConfigList -> String -> IOSArrow b String
applicationTitle cfg src
    = configSysVars cfg                                           -- (0)
      >>>
      readDocument [] src
      >>>
      (deep ( isElem >>> hasName "title" >>> getChildren >>> getText))

application     :: SysConfigList -> String -> IOSArrow b String
application cfg src
    = configSysVars cfg                                           -- (0)
      >>>
      readDocument [] src
      >>>
      processDocumentRootElement
      >>>
--      writeDocumentToString [withOutputPLAIN ] -- dst                                        -- (3)
--      >>>
      readFromString [withParseHTML yes]
      >>>
      (deep ( isElem >>> hasClass "mensa_day_speise_name" >>> getChildren >>> getText))
      -- >>>
      -- writeDocumentToString [withOutputPLAIN ] -- dst                                        -- (3)
--      >>>
--      getErrStatus

hasClass cls = hasAttrValue "class" chk
  where
    chk s = cls `elem` (words s)

processDocumentRootElement      :: IOSArrow XmlTree String
processDocumentRootElement      = (deep ( isElem >>> hasName "description" >>> getChildren >>> getText ))
--      >>> withTraceLevel 4 (traceValue 1 show)
 

--    return ()

