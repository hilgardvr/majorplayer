{-# LANGUAGE OverloadedStrings #-}

module GLDApiLeaderboard
( ApiLeaderboardResponse(..)
, ApiLeaderboardGolfer(..)
, ApiLeaderboard(..)
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import GLDApiMeta (ApiMeta(..))
import Fixture (Fixture)

data ApiLeaderboardResponse = ApiLeaderboardResponse
    { meta :: !ApiMeta
    , results :: !ApiLeaderboard
    } deriving (Show)

instance FromJSON ApiLeaderboardResponse where
    parseJSON = withObject "ApiLeaderboardResponse"  $ \v -> ApiLeaderboardResponse
        <$> v .: "meta"
        <*> v .: "results"

data ApiLeaderboard = ApiLeaderboard
    { leaderboard :: ![ApiLeaderboardGolfer]
    -- , tournament :: !ApiTournament
    } deriving (Show)

instance FromJSON ApiLeaderboard where
    parseJSON = withObject "ApiLeaderboard" $ \v -> ApiLeaderboard
        <$> v .: "leaderboard"
        -- <*> v .: "tournament"
        
data ApiTournament = ApiTournament
    { tournamentCountry :: String
    , course :: String
    , coursePar :: Int
    --, end_date :: LocalTime
    , id :: Int
    -- , liveDetails :: ApiLiveTournamentDetails
    , name :: String
    -- , start_date :: LocalTime
    , timezone :: String
    , tourId :: Int
    , tournamentType :: String
    } deriving (Show)

instance FromJSON ApiTournament where
    parseJSON = withObject "ApiTournament" $ \v -> ApiTournament
        <$> v .: "country"
        <*> v .: "course"
        <*> v .: "course_par"
        -- <*> v .: end_date
        <*> v .: "id"
        -- <*> v .: "live_details"
        <*> v .: "name"
        -- <*> v .: start_date
        <*> v .: "timezone"
        <*> v .: "tour_id"
        <*> v .: "type"

data ApiLiveTournamentDetails = ApiLiveTournamentDetails
    { liveCurrentRound :: Int
    , players :: Int
    , liveStatus :: String
    , totalRounds :: Int
    --, updated :: LocalTime
    } deriving (Show)

instance FromJSON ApiLiveTournamentDetails where
    parseJSON = withObject "ApiLiveTournamentDetails" $ \v -> ApiLiveTournamentDetails
        <$> v .: "current_round"
        <*> v .: "players"
        <*> v .: "status"
        <*> v .: "total_rounds"

data ApiLeaderboardGolferRounds = ApiLeaderboardGolferRounds
    { courseNumber :: !Int
    , positionRound :: !Int
    , roundNumber :: !Int
    , roundStrokes :: !Int
    , teeTimeLocal :: !String
    , roundTotalToPar :: !Int
    -- , roundUpdated :: !LocalTime
    } deriving (Show)

instance FromJSON ApiLeaderboardGolferRounds where
    parseJSON = withObject "ApiLeaderboardGolferRounds" $ \v -> ApiLeaderboardGolferRounds
        <$> v .: "course_number"
        <*> v .: "position_round"
        <*> v .: "round_number"
        <*> v .: "strokes"
        <*> v .: "tee_time_local"
        <*> v .: "total_to_par"
        -- <*> v .: "updated"

data ApiLeaderboardGolfer = ApiLeaderboardGolfer
    { country :: !String
    , currentRound :: !Int
    , firstName :: !String
    , holesPlayed :: !Int
    , lastName :: !String
    , playerId :: !Int
    , position :: !Int
    -- , rounds :: ![ApiLeaderboardGolferRounds]
    , status :: !String
    , strokes :: !Int 
    , totalToPar :: !Int
    -- , updated :: !LocalTime
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
        -- <*> v .: "rounds"
        <*> v .: "status"
        <*> v .: "strokes"
        <*> v .: "total_to_par"
        -- <*> v .: "updated"

