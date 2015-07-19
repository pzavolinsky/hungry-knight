{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.String (fromString)
import           Knight
import           Control.Monad.IO.Class

main :: IO ()
main = quickHttpServe site

file :: MonadSnap m => String -> String -> m ()
file filePath contentType = route [ (apiPath, handler) ]
  where apiPath = fromString(filePath)
        handler = serveFileAs (fromString contentType) ("../ui/"++filePath)

site :: Snap ()
site =
    route [ ("api/board", chessHandler) ]
    <|> ifTop       (serveFileAs "text/html" "../ui/index.html")
    <|> file "tour.html" "text/html"
    <|> dir "css"   (serveDirectory "../ui/css")
    <|> dir "fonts" (serveDirectory "../ui/fonts")
    <|> dir "js"    (serveDirectory "../ui/js")

listToJson :: Char -> Char
listToJson '(' = '['
listToJson ')' = ']'
listToJson c = c

chessHandler :: Snap ()
chessHandler = do
    modifyResponse $ setHeader "Content-Type" "application/json"
    l <- liftIO $ randomBoardIO
    writeBS $ fromString $ fmap listToJson (show l)
