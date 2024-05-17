{-# LANGUAGE OverloadedStrings #-}
module Jobs
( executeScheduleJobs )
where
import System.Cron.Schedule (execSchedule, MonadSchedule (addJob))
import Env (Env (logger), LogLevel (DEBUG))
import DataClient (DataClientApi (refreshRankings, refreshLeaderboard, refreshFixtures))
import Data.Text (Text)
import Control.Concurrent (ThreadId)

rankingRefreshSchedule :: Text
rankingRefreshSchedule = "0 12 * * 1-3"

leaderboardRefreshSchedule :: Text
leaderboardRefreshSchedule = "0 16-23/4 * * 4-7"

leaderboardRefreshSchedule2 :: Text
leaderboardRefreshSchedule2 = "0 0-12/4 * * 1"

leaderboardRefreshSchedule3 :: Text
leaderboardRefreshSchedule3 = "0 0-5/4 * * 5-7"

fixtureRefreshSchedule :: Text
fixtureRefreshSchedule = "0 18 * * 1"

executeScheduleJobs :: DataClientApi a => Env -> a -> IO [ThreadId]
executeScheduleJobs env client = do
    tids <- execSchedule $ do
        addJob (refreshRankings client) rankingRefreshSchedule
        addJob (refreshLeaderboard client) leaderboardRefreshSchedule
        addJob (refreshLeaderboard client) leaderboardRefreshSchedule2
        addJob (refreshLeaderboard client) leaderboardRefreshSchedule3
        addJob (refreshFixtures client) fixtureRefreshSchedule
    logger env DEBUG $ "starting crons: " ++ show tids
    return tids
