module DataClient
( DataClientApi(..)
) where
import Golfer (Golfer)
import Leaderboard (LeaderboardGolfer)
import Fixture (Fixture, FixtureId, NotStartedFixture, StartedFixture)

class DataClientApi a where
    getGolferRankings :: a -> IO [Golfer]
    getFixures :: a -> IO [Fixture]
    getFixtureLeaderboard :: a -> FixtureId -> IO [LeaderboardGolfer]
    getPrePostStartDate :: a -> IO (Maybe NotStartedFixture, Maybe StartedFixture)

