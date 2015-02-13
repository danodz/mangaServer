{-# LANGUAGE DeriveGeneric #-}

module MangaDocument where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import CouchUtils (couchGet)
import Data.Maybe

data MangaDoc = MangaDoc { _id :: !Text
                          , _rev :: !Text
                          , ignore :: Bool
                          , src :: !Text
                         } deriving (Show, Generic)
instance FromJSON MangaDoc
instance ToJSON MangaDoc

data NameToIgnore = NameToIgnore { id :: !Text
                                 , key :: !Text
                                 , value :: Bool
                                 } deriving (Show, Generic)
instance FromJSON NameToIgnore
instance ToJSON NameToIgnore

data NameToIgnoreView = NameToIgnoreView { total_rows :: Int
                                         , offset :: Int
                                         , rows :: [NameToIgnore]
                                         } deriving (Show, Generic)
instance FromJSON NameToIgnoreView
instance ToJSON NameToIgnoreView

nameToIgnore :: IO ([NameToIgnore])
nameToIgnore = do 
    doc <- couchGet "manga/_design/manga/_view/nameToIgnore" :: IO (Maybe NameToIgnoreView)
    return $ rows $ fromJust doc
