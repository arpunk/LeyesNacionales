{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Conduit as C
import qualified Data.Text as T
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (concat, putStrLn)
import Data.Text (concat)
import Data.Text.IO (putStrLn)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attribute, attributeIs, element, fromDocument, ($//), (&//), (&/), (&|), (>=>))

-- FIXME: Evolucionar la prueba de concepto...
url = "http://wsp.presidencia.gov.co/Normativa/Leyes/Paginas/2013.aspx"
urlBase = "http://wsp.presidencia.gov.co"

findNodes :: Cursor -> [Cursor]
findNodes = element "div" >=> attributeIs "class" "link-item" &/ element "a"

extractData :: Cursor -> T.Text
extractData = concat . attribute "href"

processData :: [T.Text] -> IO ()
processData = mapM_ downloadPdf


split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
  | c == delim = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs delim

createFileName :: String -> String
createFileName raw = last $ split raw '/'

downloadPdf :: T.Text -> IO ()
downloadPdf link = C.runResourceT $ do
  manager <- liftIO $ newManager conduitManagerSettings
  req <- liftIO $ parseUrl (T.unpack $ T.append urlBase link)
  res <- http req manager
  responseBody res C.$$+- sinkFile $ createFileName (T.unpack link)

cursorFor :: String -> IO Cursor
cursorFor u = do
  page <- simpleHttp u
  return $ fromDocument $ parseLBS page

main :: IO ()
main = do
  cursor <- cursorFor url 
  processData $ cursor $// findNodes &| extractData
