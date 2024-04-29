module Main where
import Network.Wai.Middleware.RequestLogger (logStdout)
import LoginController (loginRoutes)
import TeamController (teamRoutes)
import Web.Scotty (scotty, middleware)
import LeagueController (leagueRoutes)
import Env (Env, getAppEnv)
import GLDApiClient (getGLDClient)
import DataClient (DataClientApi(getGolferRankings))

app :: Env -> IO ()
app env = do
    let client = getGLDClient env
    allGolfers <- getGolferRankings client
    scotty 3000 $ do
        middleware logStdout
        teamRoutes env allGolfers client
        leagueRoutes env allGolfers client
        loginRoutes env allGolfers


main :: IO ()
main = do
    env <- getAppEnv
    app env
