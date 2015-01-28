--http://mangafox.me/manga/fairy_tail/v36/c301/4.html
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Text (Text, pack, unpack)
import Data.Aeson
import GHC.Generics
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

import CouchUtils as C

import MangafoxParser
import ChapterDocument as CD
import ChapterListDocument as CID
import MangaDocument as MD
import Utils

htmlTemplate :: String -> H.Html -> H.Html
htmlTemplate title body = 
    H.html $ do
        H.head $ do
            H.title $ H.toHtml title
        H.body body

getManga :: String -> IO (Maybe String)
getManga name = do
--    Network.HTTP.simpleHTTP (getRequest $ "http://mangafox.me/manga/" ++ name) >>= getResponseBody
    answer <- maybeHTTP ("http://mangafox.me/manga/" ++ name) 10
    case answer of
        Nothing -> return Nothing
        _ -> return answer

getChapterDoc :: String -> IO (Maybe Chapter)
getChapterDoc id = ( couchGet $ "manga/" ++ (replace "/" "%2F" id) ) :: IO (Maybe Chapter)

--getChapter :: String -> String -> Int -> String
--getChapter html name index
--    | index < 0 = (chapterList html name) !! 0
--    | index > ( length $ chapterList html name ) = (chapterList html name) !! ( ( length $ chapterList html name ) - 1 )
--    | otherwise = (chapterList html name) !! index
--
--viewChapterOld :: String -> ServerPartT IO HS.Response
--viewChapterOld name = do
--    chapter <- optional $ look "chapter"
--    manga <- liftIO $ getManga name
--    mangaHtml <- liftIO $ getManga name
--    viewChapterOld' name chapter manga ( length $ chapterList mangaHtml name )
--        where
--            viewChapterOld' :: String -> Maybe String -> String -> Int -> ServerPartT IO HS.Response
--            viewChapterOld' name chapter manga chapterCount
--                | ( read $ fromMaybe "0" chapter ) < 0 = seeOther (name ++ "?chapter=0") $ toResponse ()
--                | ( read $ fromMaybe "0" chapter ) > chapterCount - 1 = do
--                    seeOther (name ++ "?chapter=" ++ ( show $ chapterCount - 1 )) $ toResponse ()
--                | otherwise = do
--                    images <- liftIO $ chapterImages name ( getChapter manga name $ read $ fromMaybe "0" chapter ) []
--                    ok $ toResponse $
--                        htmlTemplate "Manga reader" $ do
--                            H.div $ do
--                                forM_ images ( \x ->
--                                    H.img ! A.src ( stringValue x ) )
--                            H.a ! A.href ( stringValue $ name ++ "?chapter=" ++ (show $ (read $ fromMaybe "0" chapter) - 1 ) ) $
--                                ( H.toHtml ("next chapter" :: String) )

viewChapter :: String -> ServerPartT IO HS.Response
viewChapter name = do
    chapter <- look "chapter"
    chapterDoc <- liftIO ( (C.couchGet $ "manga/" ++ (replace "/" "%2F" chapter)) :: IO (Maybe CD.Chapter) )
    chapters <- liftIO (C.couchGet $ "manga/_design/chapter/_view/mangaToChapter?key=%22" ++ name ++ "%22" :: IO (Maybe CD.MangaChapterRows))
    void $ liftIO $ changeIsRead chapter True
    ok $ toResponse $
        htmlTemplate "Manga reader" $ do
            navLink "Previous" $ maybeListGet ( CD.rows $ fromJust chapters ) $ ( fromJust $ elemIndex (MangaToChapter (CD._id $ fromJust chapterDoc) (pack name) (fromJust chapterDoc)) $ CD.rows $ fromJust chapters ) - 1
            navLink "Next" $ maybeListGet ( CD.rows $ fromJust chapters ) $ ( fromJust $ elemIndex (MangaToChapter (CD._id $ fromJust chapterDoc) (pack name) (fromJust chapterDoc)) $ CD.rows $ fromJust chapters ) + 1
            H.a ! ( A.href $ stringValue $ "/manga/" ++ name ) $ H.toHtml $ "Back to " ++ name
            H.a ! ( A.href $ stringValue "/manga/" ) $ H.toHtml ( "Back to all manga" :: String )
            H.div $ do
                forM_ (CD.pages $ fromJust chapterDoc) ( \x ->
                    H.img ! A.src ( stringValue $ init $ tail $ show x ) )
            navLink "Previous" $ maybeListGet ( CD.rows $ fromJust chapters ) $ ( fromJust $ elemIndex (MangaToChapter (CD._id $ fromJust chapterDoc) (pack name) (fromJust chapterDoc)) $ CD.rows $ fromJust chapters ) - 1
            navLink "Next" $ maybeListGet ( CD.rows $ fromJust chapters ) $ ( fromJust $ elemIndex (MangaToChapter (CD._id $ fromJust chapterDoc) (pack name) (fromJust chapterDoc)) $ CD.rows $ fromJust chapters ) + 1
            H.a ! ( A.href $ stringValue $ "/manga/" ++ name ) $ H.toHtml $ "Back to " ++ name
            H.a ! ( A.href $ stringValue "/manga/" ) $ H.toHtml ( "Back to all manga" :: String )

    where
      navLink :: String -> Maybe MangaToChapter -> H.Html
      navLink text Nothing = H.a ! A.disabled "" $ H.toHtml text
      navLink text chapter = H.a ! (A.href $ stringValue $ "?chapter=" ++ (unpack $ CD.id $ fromJust chapter)) $ H.toHtml text

mangaPage :: String -> ServerPartT IO HS.Response
mangaPage name = do
    chapters <- liftIO (C.couchGet $ "manga/_design/chapter/_view/mangaToChapter?key=%22" ++ name ++ "%22" :: IO (Maybe CD.MangaChapterRows))
    ok $ toResponse $
        H.html $ do
            H.head $ do
                H.title $ H.toHtml name
                H.script ! A.src "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js" $ ""
                H.script ! A.src "/files/mangaPage.js" $ ""
            H.body $ do 
                H.a ! ( A.href $ stringValue "/manga" ) $ H.toHtml ( "Back to all manga" :: String )
                H.button ! A.onclick "submitReadChange()" $ "submit read change"
                H.button ! A.onclick "$('.readState').each(function(x,y){y.checked=true})" $ "select all"
                H.button ! A.onclick "$('.readState').each(function(x,y){y.checked=false})" $ "deselect all"
                H.div $ do
                    forM_ ( reverse $ CD.rows $ fromJust chapters ) ( \chapter ->
                        H.div $ do
                            makeCheckbox $ CD.value chapter
                            H.a ! A.href ( stringValue $ "/chapter/" ++ name ++ "?chapter=" ++ ( init $ tail $ show $ CD.id chapter ) ) $
                                H.toHtml $ init $ tail $ show $ CD.id chapter )
                H.a ! ( A.href $ stringValue "/manga" ) $ H.toHtml ( "Back to all manga" :: String )
                H.button ! A.onclick "submitReadChange()" $ "submit read change"
                H.button ! A.onclick "$('.readState').each(function(x,y){y.checked=true})" $ "select all"
                H.button ! A.onclick "$('.readState').each(function(x,y){y.checked=false})" $ "deselect all"
    where
      makeCheckbox chapter = 
          if CD.isRead chapter then
              H.input ! A.type_ "checkbox" ! A.checked "" ! A.class_ "readState"
          else
              H.input ! A.type_ "checkbox" ! A.class_ "readState"

allMangasPage :: ServerPartT IO HS.Response
allMangasPage = do
    mangaList <- liftIO (C.couchGet "manga/_design/chapter/_view/mangaToCount?group=true" :: IO (Maybe MD.MangaCountRows))
    addHeaderM "Access-Control-Allow-Origin" "*"
    ok $ toResponse $
        H.html $ do
            H.head $ do
                H.title $ "All mangas"
                H.script ! A.src "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js" $ ""
                H.script ! A.src "/files/allMangaPage.js" $ ""
            H.body $ do 
                H.div $ do
                    H.input ! A.class_ "newManga"
                    H.button "Submit" ! A.onclick "submitNew()"
                mangaDiv $ filter (\x -> ( MD.unread $ MD.value x ) /= 0) $ MD.rows $ fromJust $ mangaList 
                H.br
                mangaDiv $ filter (\x -> ( MD.unread $ MD.value x ) == 0) $ MD.rows $ fromJust $ mangaList 
    where
      mangaDiv mangaList = 
          H.div $ do
              forM_ ( mangaList ) (\x -> 
                  H.div $ do
                      H.a ! A.href ( stringValue $ "/manga/" ++ (unpack $ MD.key x) ) $
                          H.toHtml $ (unpack $ MD.key x) ++ " : " ++ (show $ MD.total $ MD.value x) ++ " (" ++ (show $ MD.unread $ MD.value x) ++ ")")
          

updateMangas :: IO ()
updateMangas = forever $ do
    localList <- liftIO ( couchGet "manga/_design/chapter/_view/mangaToChapterList?group=true"  :: IO (Maybe CID.ChapterListRows) )
    print "###########################  UPDATING  ###########################"
    void $ liftIO $ mapM_ updateManga $ CID.rows $ fromJust localList
    threadDelay 1800000000
    return ()

updateManga :: CID.MangaToChapterList -> IO ()
updateManga doc = 
    updateManga' (map unpack $ CID.value doc ) ( unpack $ CID.key doc )
    where
      updateManga' :: [String] -> String -> IO ()
      updateManga' localList name = do
          print name
          manga <- getManga name

          case manga of
              Nothing -> return ()
              _ -> do
                  ( mapM (\x -> forkIO $ appendChapter name x ) ) $
                      ( chapterList ( fromJust manga ) name \\ localList )
                  return ()

newMangaHandler :: String -> ServerPartT IO HS.Response
newMangaHandler name = do
    mangaPage <- liftIO $ getManga name
    checkAnswer <- liftIO $ pageCheck ( fromJust mangaPage ) name
    ok $ toResponse $
        htmlTemplate "mangaAnswer" $
            H.toHtml checkAnswer
    where
      pageCheck :: String -> String -> IO String
      pageCheck page name =
        if page /= "" then do
            answer <- addNewMangaCouch page name
            return answer
        else
            return ("manga not found" :: String)

addNewMangaCouch :: String -> String -> IO String
addNewMangaCouch page name = do
    forkIO $ fillChapters page name
    return ("Manga found" :: String)

fillChapters :: String -> String -> IO ()
fillChapters page name = do
    mapM (appendChapter name) $ reverse $ chapterList page name
    return ()

appendChapter :: String -> String -> IO ()
appendChapter mangaName firstPage = do 
    print $ mangaName ++ " : " ++ firstPage
    chapter <- makeChapter mangaName firstPage
    print chapter
    C.couchPut "manga" $ encode chapter
    return ()

changeIsRead :: String -> Bool -> IO ()
changeIsRead chapterId value = do
    doc <- getChapterDoc chapterId
    C.couchPut "manga" $ encode $ (fromJust doc) { isRead = value }
    return ()

changeRead :: ServerPartT IO HS.Response
changeRead = do
    changeList <- look "changeList"
    liftIO $ mapM_ (\(x,y) -> changeIsRead x y) $ ( read changeList :: [(String,Bool)] )
    ok $ toResponse $ ("OK" :: String)

main = do 
    forkIO $ updateMangas
    Happstack.Server.simpleHTTP nullConf $
        msum
        [
         dir "chapter" $ path $ viewChapter
        ,dir "manga" $ path $ mangaPage
        ,dir "manga" $ allMangasPage
        ,dir "addManga" $ path $ newMangaHandler
        ,dir "changeRead" $ changeRead
        ,dir "files" $ serveDirectory DisableBrowsing [] "files"
--       ,dir "oldChapter" $ path $ viewChapterOld
        ]
