{-# LANGUAGE OverloadedStrings #-}

module MangafoxParser where

import Data.Maybe
import Data.Text (Text, pack, unpack)
import Control.Exception
import Control.Error
import Data.List.Split
import Data.List
import Data.Char
import Network.CGI (liftIO)
import Network.HTTP
import CouchUtils

<<<<<<< HEAD
import MangaDocument as MD
import ChapterDocument as CD
=======
import MangaDocument
import ChapterDocument
>>>>>>> 6c80267a0aa1d81bc3bce763ff5f9b2cb0002211
import Utils

baseUrl :: String
baseUrl = "http://mangafox.me/manga/"

chapterList :: String -> String -> [String]
chapterList page name = 
    ( map (\x -> x ++ "1.html" ) ) .
    ( map (\x -> head $ splitOn "1.html" x ) ) .
    ( map (\x -> last $ splitOn baseUrl x ) ) .
    ( map (\x -> head $ splitOn "\"" x) ) .
    ( map (\x -> last $ splitOn "href=\"" x) ) .
    ( init ) .
    ( splitOn "\" class=\"tips\"" ) $
    page

getImgUrl :: String -> String
getImgUrl page =
        ( head ) .
        ( splitOn "\"" ) .
        ( last ) .
        ( splitOn "<img src=\"" ) .
        ( head ) .
        ( splitOn "id=\"image\"" ) $
        page

getNextPageUrl :: String -> String
getNextPageUrl page =
    ( head ) .
    ( splitOn "\"" ) .
    ( last ) .
    ( splitOn "<a href=\"" ) .
    ( head ) .
    ( splitOn "id=\"image\"" ) $
    page

chapterImages :: String ->  String -> [String] -> IO [String]
chapterImages manga url imgs
    | ( length $ splitOn "javascript" url ) > 1 = do return imgs
    | ( length $ splitOn "forums.mangafox.me" url ) > 1 = do return imgs
    | otherwise = do
        html <- maybeHTTP (baseUrl ++ url) 10
        chapterImages manga ( (getDirPath url) ++ (getNextPageUrl $ fromJust html) ) (imgs ++ [getImgUrl $ fromJust html])

makeChapter :: String -> String -> IO Chapter
makeChapter manga firstPage = do
    images <- chapterImages manga firstPage []
<<<<<<< HEAD
    return Chapter { CD._id = pack $ firstPage
                   , CD._rev = emptyRev
=======
    return Chapter { _id = pack $ firstPage
                   , _rev = emptyRev
>>>>>>> 6c80267a0aa1d81bc3bce763ff5f9b2cb0002211
                   , manga = pack manga
                   , pages = map pack images
                   , isRead = False
                   }

allChapters :: [IO Chapter] -> [Chapter] -> IO [Chapter]
allChapters [] storeChapters = return storeChapters
allChapters (chapter:body) storeChapters = do
    cleanChapter <- chapter
    allChapters body $ cleanChapter : storeChapters
