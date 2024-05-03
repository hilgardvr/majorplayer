module DataClient
( DataClientApi(..)
) where
import Golfer (Golfer)
import Leaderboard (LeaderboardGolfer)
import Fixture (Fixture, FixtureId)
import Data.Time (LocalTime)

class DataClientApi a where
    getGolferRankings :: a -> IO [Golfer]
    getFixures :: a -> IO [Fixture]
    getCurrentFixture :: a -> IO Fixture
    getFixtureLeaderboard :: a -> FixtureId -> IO [LeaderboardGolfer]
    isFixtureRunning :: a -> Fixture -> LocalTime -> Bool

