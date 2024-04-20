module Main where
import Network.Wai.Middleware.RequestLogger (logStdout)
import LoginController (loginRoutes)
import TeamController (teamRoutes)
import Web.Scotty (scotty, middleware)
import LeagueController (leagueRoutes)
import Env (Env, getAppEnv)
import GLDApiClient (getGLDClient)
--import Fixture (currentFixture, Fixture (id))
--import Data.Time (getCurrentTime, utc, utcToLocalTime)
import DataClient (DataClientApi(getGolferRankings, getFixures, getFixtureLeaderboard))

app :: Env -> IO ()
app env = do
    let client = getGLDClient env
    allGolfers <- getGolferRankings client
    --fs <- getFixures client
    --nowUtc <- getCurrentTime
    --let nowUtcLocal = utcToLocalTime utc nowUtc
    --    cf = currentFixture fs nowUtcLocal
    --leaderBoard <- getFixtureLeaderboard client (Fixture.id cf)
    --print leaderBoard
    scotty 3000 $ do
        middleware logStdout
        loginRoutes env allGolfers
        teamRoutes env allGolfers client
        leagueRoutes env allGolfers


main :: IO ()
main = do
    env <- getAppEnv
    app env
