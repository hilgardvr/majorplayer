{-# LANGUAGE OverloadedStrings #-}

module GLDApiRankings
( RankingApiResponse(..)
, ApiRankings(..)
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import GLDApiMeta (ApiMeta(..))
import GLDApiGolfer (ApiGolfer)


data ApiRankings = ApiRankings
    { rankings :: [ApiGolfer]
    } deriving (Show)

instance FromJSON ApiRankings where
    parseJSON = withObject "ApiRankings" $ \v -> ApiRankings
        <$> v .: "rankings"

data RankingApiResponse = RankingApiResponse
    { meta :: !ApiMeta
    , results :: !ApiRankings
    } deriving (Show)

instance FromJSON RankingApiResponse where
    parseJSON = withObject "RankingApiResponse"  $ \v -> RankingApiResponse
        <$> v .: "meta"
        <*> v .: "results"

