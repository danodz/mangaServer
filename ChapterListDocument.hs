{-# LANGUAGE DeriveGeneric #-}

module ChapterListDocument where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data MangaToChapterList = ChapterId { key :: !Text
                                    , value :: [Text]
                                    } deriving (Show, Generic)
instance FromJSON MangaToChapterList
instance ToJSON MangaToChapterList

data ChapterListRows = ChapterListRows { rows :: [MangaToChapterList]
                                       } deriving (Show, Generic)
instance FromJSON ChapterListRows
instance ToJSON ChapterListRows
