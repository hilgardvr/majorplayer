{-# LANGUAGE OverloadedStrings #-}

module GolfLeaderboardDataAPIResponse
( --ApiResponse(..)
ApiMeta(..)
, RawApiResponse(..)
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Database.PostgreSQL.Simple (ToRow, FromRow)
import Data.UUID (UUID)
import Data.Time (LocalTime)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField(toField))

--data ApiResponse r = ApiResponse
--    { meta :: ApiMeta
--    , results :: r
--    } deriving (Show)

data ApiMeta = ApiMeta
    { title :: !String
    , description :: !String
    } deriving (Show)

instance FromJSON ApiMeta where
    parseJSON = withObject "ApiMeta" $ \v -> ApiMeta
        <$> v .: "title"
        <*> v .: "description"

data RawApiResponse = RawApiResponse 
    { responseId :: !(Maybe UUID)
    , rawResponse :: !String
    , createdAt :: !LocalTime
    }

instance ToRow RawApiResponse where
    toRow (RawApiResponse _ rr _) = [toField rr]

instance FromRow RawApiResponse where
    fromRow = RawApiResponse <$> field <*> field <*> field

