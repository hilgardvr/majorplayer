module Main where
import Network.Wai.Middleware.RequestLogger (logStdout)
import LoginController (loginRoutes)
import TeamController (teamRoutes)
import Web.Scotty (scotty, middleware)
import LeagueController (leagueRoutes)
import Env (Env, getAppEnv)
import GLDApiClient (getGolferRankings, getFixures)
import Fixture (currentFixture)
import Data.Time (getCurrentTime, utc, utcToLocalTime)

app :: Env -> IO ()
app env = do
    allGolfers <- getGolferRankings env
    fs <- getFixures env
    nowUtc <- getCurrentTime
    let nowUtcLocal = utcToLocalTime utc nowUtc
        cf = currentFixture fs nowUtcLocal
    print cf
    scotty 3000 $ do
        middleware logStdout
        loginRoutes env allGolfers
        teamRoutes env allGolfers
        leagueRoutes env allGolfers


main :: IO ()
main = do
    env <- getAppEnv
    app env
