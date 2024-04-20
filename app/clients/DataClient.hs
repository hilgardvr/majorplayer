module DataClient
( DataClientApi(..)
) where
import Golfer (Golfer)
import Leaderboard (LeaderboardGolfer)
import Fixture (Fixture, FixtureId)

class DataClientApi a where
    getGolferRankings :: a -> IO [Golfer]
    getFixures :: a -> IO [Fixture]
    getFixtureLeaderboard :: a -> FixtureId -> IO [LeaderboardGolfer]

