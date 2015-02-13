{-# LANGUAGE DeriveGeneric #-}

module MangaView where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data MangaCountRows = MangaCountRows { rows :: [MangaToCount]
                                     } deriving (Show, Generic)
instance FromJSON MangaCountRows
instance ToJSON MangaCountRows

data MangaToCount = MangaToCount { key :: !Text
                                    , value :: CountDoc
                                    } deriving (Show, Generic)
instance FromJSON MangaToCount
instance ToJSON MangaToCount

data CountDoc = CountDoc { total :: Int
                         , unread :: Int
                         } deriving (Show, Generic)
instance FromJSON CountDoc
instance ToJSON CountDoc
