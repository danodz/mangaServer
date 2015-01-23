{-# LANGUAGE DeriveGeneric #-}

module ChapterDocument where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data Chapter = Chapter { _id :: !Text
                       , _rev :: !Text
                       , manga :: !Text
                       , pages :: [Text]
                       , isRead :: Bool
                       } deriving (Show, Generic)
instance FromJSON Chapter
instance ToJSON Chapter

data MangaToChapter = MangaToChapter { id :: !Text
                                     , key :: !Text
                                     , value :: Chapter
                                     } deriving (Show, Generic)
instance FromJSON MangaToChapter
instance ToJSON MangaToChapter
instance Eq MangaToChapter where
    x == y = (ChapterDocument.id x) == (ChapterDocument.id y)

data MangaChapterRows = MangaChapterRows { total_rows :: Int
                                         , offset :: Int
                                         , rows :: [MangaToChapter]
                                         } deriving (Show, Generic)
instance FromJSON MangaChapterRows
instance ToJSON MangaChapterRows
