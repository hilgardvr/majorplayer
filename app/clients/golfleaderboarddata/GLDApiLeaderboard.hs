{-# LANGUAGE OverloadedStrings #-}

module GLDApiLeaderboard
( ApiLeaderboardResponse(..)
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import GLDApiMeta (ApiMeta(..))
import Fixture (Fixture)
import Data.Time (LocalTime)

data ApiLeaderboardGolferRounds = ApiLeaderboardGolferRounds
    { courseNumber :: Int
    , positionRound :: Int
    , roundNumber :: Int
    , roundStrokes :: Int
    , teeTimeLocal :: String
    , roundTotalToPar :: Int
    , roundUpdated :: LocalTime
    } deriving (Show)

instance FromJSON ApiLeaderboardGolferRounds where
    parseJSON = withObject "ApiLeaderboardGolferRounds" $ \v -> ApiLeaderboardGolferRounds
        <$> v .: "course_number"
        <*> v .: "position_round"
        <*> v .: "round_number"
        <*> v .: "strokes"
        <*> v .: "tee_time_local"
        <*> v .: "total_to_par"
        <*> v .: "updated"

data ApiLeaderboardGolfer = ApiLeaderboardGolfer
    { country :: String
    , currentRound :: Int
    , firstName :: String
    , holesPlayed :: Int
    , lastName :: String
    , playerId :: Int
    , position :: Int
    , rounds :: [ApiLeaderboardGolferRounds]
    , status :: String
    , strokes :: Int 
    , totalToPar :: Int
    , updated :: LocalTime
    } deriving (Show)

instance FromJSON ApiLeaderboardGolfer where
    parseJSON = withObject "ApiLeaderboardGolfer" $ \v -> ApiLeaderboardGolfer
        <$> v .: "country"
        <*> v .: "current_round"
        <*> v .: "first_name"
        <*> v .: "holes_played"
        <*> v .: "last_name"
        <*> v .: "player_id"
        <*> v .: "position"
        <*> v .: "rounds"
        <*> v .: "status"
        <*> v .: "strokes"
        <*> v .: "total_to_par"
        <*> v .: "updated"

data ApiLeaderboard = ApiLeaderboard
    { leaderboard :: ![ApiLeaderboardGolfer]
    , tournament :: !Fixture
    } deriving (Show)

instance FromJSON ApiLeaderboard where
    parseJSON = withObject "ApiLeaderboard" $ \v -> ApiLeaderboard
        <$> v .: "leaderboard"
        <*> v .: "tournament"

data ApiLeaderboardResponse = ApiLeaderboardResponse
    { meta :: !ApiMeta
    , results :: !ApiLeaderboard
    } deriving (Show)

instance FromJSON ApiLeaderboardResponse where
    parseJSON = withObject "ApiLeaderboardResponse"  $ \v -> ApiLeaderboardResponse
        <$> v .: "meta"
        <*> v .: "results"

