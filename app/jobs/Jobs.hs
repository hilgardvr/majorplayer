{-# LANGUAGE OverloadedStrings #-}
module Jobs
( executeScheduleJobs )
where
import System.Cron.Schedule (execSchedule, MonadSchedule (addJob))
import Env (Env (logger), LogLevel (DEBUG, INFO))
import DataClient (DataClientApi (getGolferRankings))
import Data.Text (Text)
import Control.Concurrent (ThreadId)

rankingRefreshSchedule :: Text
--rankingRefreshSchedule = "* 12 * * 1-3"
rankingRefreshSchedule = "* * * * 1-3"

executeScheduleJobs :: DataClientApi a => Env -> a -> IO [ThreadId]
executeScheduleJobs env client = do
    tids <- execSchedule $ do
        addJob (print "sdcheduled job.....") rankingRefreshSchedule
        --addJob (rankingRefresh env client) rankingRefreshSchedule
    logger env DEBUG $ "starting crons: " ++ show tids
    return tids

        

rankingRefresh :: DataClientApi a =>  Env -> a -> IO ()
rankingRefresh env client = do
    getGolferRankings client >> logger env INFO "Refreshed golfer rankings"

    
    
