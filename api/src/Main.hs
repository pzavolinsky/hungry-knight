{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.String (fromString)
import           Knight
import           Tour (getTour)
import           Control.Monad.IO.Class
import           Data.ByteString.Char8
import           Text.Read

main :: IO ()
main = quickHttpServe site

file :: MonadSnap m => String -> String -> m ()
file filePath contentType = route [ (apiPath, handler) ]
  where apiPath = fromString(filePath)
        handler = serveFileAs (fromString contentType) ("../ui/"++filePath)

site :: Snap ()
site =
    ifTop     (serveFileAs "text/html" "../ui/index.html")
    <|> route [ ("api/puzzle/path", puzzleHandler) ]
    <|> route [ ("api/tour/:size/:pos", tourHandler) ]
    <|> file  "tour.html" "text/html"
    <|> file  "path.html" "text/html"
    <|> dir   "css"   (serveDirectory "../ui/css")
    <|> dir   "fonts" (serveDirectory "../ui/fonts")
    <|> dir   "js"    (serveDirectory "../ui/js")

-- API Boilerplate

listToJson :: Char -> Char
listToJson '(' = '['
listToJson ')' = ']'
listToJson c = c

returnList :: Show a => [a] -> Snap ()
returnList a = writeBS $ fromString $ fmap listToJson (show a)

badRequest :: ByteString -> Snap ()
badRequest msg = do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    writeBS $ "Bad Request: " `append` msg

tryReadInt :: ByteString -> Either ByteString Int
tryReadInt s =
    let ms = (readMaybe.unpack $ s) :: Maybe Int
    in maybe (Left $ "'" `append` s `append` "' must be a number") Right ms

tryGetParam :: ByteString -> Snap (Either ByteString ByteString)
tryGetParam name = do
    mvalue <- getParam name
    return $ maybe (Left $ "Missing parameter '" `append` name `append` "'") Right mvalue

splitParam :: Char -> ByteString -> Either ByteString (Int,Int)
splitParam delim s = do
    let items = split delim s :: [ByteString]
    case items of
        a:b:[] -> do
            ia <- tryReadInt a
            ib <- tryReadInt b
            return (ia,ib)
        _ -> Left $ "'" `append` s `append` "' must be <number>" `append` (fromString [delim]) `append` "<number>"

-- Handlers
puzzleHandler :: Snap ()
puzzleHandler = do
    modifyResponse $ setHeader "Content-Type" "application/json"
    l <- liftIO $ randomBoardIO
    returnList l


computeTour :: ((Int,Int),(Int,Int)) -> Snap ()
computeTour ((mx,my),p) = do
    let mtour = getTour mx my p
    maybe (writeBS "Not found") returnList mtour

tourHandler :: Snap ()
tourHandler = do
    modifyResponse $ setHeader "Content-Type" "application/json"
    msize <- tryGetParam "size"
    mpos <- tryGetParam "pos"
    let m = do
        ssize <- msize
        spos <- mpos
        size <- splitParam 'x' ssize
        pos <- splitParam ',' spos
        return (size, pos)
    either badRequest computeTour m
