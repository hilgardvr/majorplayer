module Leaderboard 
( LeaderboardGolfer(..)
) where

data LeaderboardGolfer = LeaderboardGolfer
    { country :: !String
    , currentRound :: !Int
    , firstName :: !String
    , holesPlayed :: !Int
    , lastName :: !String
    , playerId :: !Int
    , position :: !Int
    -- , rounds :: ![LeaderboardGolferRounds]
    , status :: !String
    , strokes :: !Int 
    , totalToPar :: !Int
    -- , updated :: !LocalTime
    } deriving (Show)

data LeaderboardGolferRounds = LeaderboardGolferRounds
    { courseNumber :: !Int
    , positionRound :: !Int
    , roundNumber :: !Int
    , roundStrokes :: !Int
    , teeTimeLocal :: !String
    , roundTotalToPar :: !Int
    -- , roundUpdated :: !LocalTime
    } deriving (Show)
