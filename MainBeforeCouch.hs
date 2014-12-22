{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import System.IO.Unsafe
import Data.Maybe
import Control.Applicative ((<$>), (<*>), optional)
import Control.Concurrent (forkIO, threadDelay)
import Happstack.Server (ServerPart, nullConf, simpleHTTP, ok, dir, path, seeOther)
import Happstack.Server as HS
import Data.List
import Data.List.Split
import Data.List.Utils
import Network.HTTP
import Network.CGI (liftIO)
import Text.Blaze ((!))
import Text.Blaze.Internal
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

htmlTemplate :: String -> H.Html -> H.Html
htmlTemplate title body = 
	H.html $ do
		H.head $ do
			H.title $ H.toHtml title
		H.body body

chapterList html name = 
	( map (\x -> last $ splitOn name x ) ).
	( map (\x -> head $ splitOn "\"" x) ) .
	( map (\x -> last $ splitOn "href=\"" x) ) .
	( init ) .
	( splitOn "\" class=\"tips\"" ) $
	html
	
getDirPath :: String -> String
getDirPath filePath =
	reverse .
	dropWhile ( \x -> x /= '/') .
	reverse $
	filePath

getImgUrl :: String -> String
getImgUrl html =
		( head ) .
		( splitOn "\"" ) .
		( last ) .
		( splitOn "<img src=\"" ) .
		( head ) .
		( splitOn "id=\"image\"" ) $
		html

getNextPageUrl :: String -> String
getNextPageUrl html =
	( head ) .
	( splitOn "\"" ) .
	( last ) .
	( splitOn "<a href=\"" ) .
	( head ) .
	( splitOn "id=\"image\"" ) $
	html

getManga :: String -> IO String
getManga name =
	Network.HTTP.simpleHTTP (getRequest $ "http://mangafox.me/manga/" ++ name) >>= getResponseBody

chapterImages :: String -> [String] -> IO [String]
chapterImages url imgs
	| ( length $ splitOn "javascript" url ) > 1 = do return imgs
	| ( length $ splitOn "forums.mangafox.me" url ) > 1 = do return imgs
	| otherwise = do
		appendFile "log.out" $ "Request to : " ++ url ++ "\n"
		html <- Network.HTTP.simpleHTTP ( getRequest url ) >>= getResponseBody
		appendFile "log.out" $ "END Request to : " ++ url ++ "\n"
		chapterImages ( (getDirPath url) ++ (getNextPageUrl html) ) (imgs ++ [getImgUrl html])

getChapter :: String -> String -> Int -> String
getChapter html name index
	| index < 0 = (chapterList html name) !! 0
	| index > ( length $ chapterList html name ) = (chapterList html name) !! ( ( length $ chapterList html name ) - 1 )
	| otherwise = (chapterList html name) !! index

viewChapter :: String -> ServerPartT IO HS.Response
viewChapter name = do
	chapter <- optional $ look "chapter"
	manga <- liftIO $ getManga name
	mangaHtml <- liftIO $ getManga name
	viewChapter' name chapter manga ( length $ chapterList mangaHtml name )
		where
			viewChapter' :: String -> Maybe String -> String -> Int -> ServerPartT IO HS.Response
			viewChapter' name chapter manga chapterCount
				| ( read $ fromMaybe "0" chapter ) < 0 = seeOther (name ++ "?chapter=0") $ toResponse ()
				| ( read $ fromMaybe "0" chapter ) > chapterCount - 1 = do
					liftIO $ print chapterCount
					seeOther (name ++ "?chapter=" ++ ( show $ chapterCount - 1 )) $ toResponse ()
				| otherwise = do
					images <- liftIO $ chapterImages ( "http://mangafox.me/manga/" ++ name ++ ( getChapter manga name $ read $ fromMaybe "0" chapter )) []
					ok $ toResponse $
						htmlTemplate "Manga reader" $ do
							H.div $ do
								forM_ images ( \x ->
									H.img ! A.src ( stringValue x ) )
							H.a ! A.href ( stringValue $ name ++ "?chapter=" ++ (show $ (read $ fromMaybe "0" chapter) - 1 ) ) $
								( H.toHtml ("next chapter" :: String) )

mangaOk :: String -> ServerPartT IO HS.Response
mangaOk name = do
	html <- liftIO $ getManga name
	ok $ toResponse $
		htmlTemplate name $ do
			H.div $ do
				forM_ ( zip [(0 :: Int)..] $ chapterList html name ) ( \(x,e) ->
					H.div $ do
						H.a ! A.href ( stringValue $ "chapter/" ++ name ++ "?chapter=" ++ ( show x ) ) $
							H.toHtml $ show $ ( length $ chapterList html name ) - x )

main = Happstack.Server.simpleHTTP nullConf $
	msum
	[
		dir "chapter" $ path $ viewChapter
		,dir "manga" $ path $ mangaOk
	]
