{-# LANGUAGE OverloadedStrings #-}

module GLDApiGolfer
( ApiGolfer(..)
, toGolfer
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Golfer (Golfer (Golfer))

type ApiRanking = Int
type ApiGolferName = String
type ApiGolferId = Int

data ApiGolfer = ApiGolfer
    { id :: !ApiGolferId
    , ranking :: !ApiRanking
    , name :: !ApiGolferName
    } deriving (Show)

instance FromJSON ApiGolfer where
    parseJSON = withObject "ApiGolfer" $ \v -> ApiGolfer
        <$> v .: "player_id"
        <*> v .: "position"
        <*> v .: "player_name"


toGolfer :: ApiGolfer -> Golfer
toGolfer (ApiGolfer i r n) = Golfer i r n False
