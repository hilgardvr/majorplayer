{-# LANGUAGE OverloadedStrings #-}

module Env
( LogLevel(..)
, Env(..)
, getAppEnv
, Season
) where
import Database.PostgreSQL.Simple (Connection)
import Data.Time (getCurrentTime)
import System.Environment (getEnv, setEnv, lookupEnv)
import Data.List (elemIndex)
import qualified Repo as R (connect)
import Data.Text (Text)

data LogLevel = DEBUG | INFO | WARN | ERROR deriving (Show)

data AppEnv = DEV | PROD deriving (Read, Show)

type Season = Int

data Env = Env 
    { conn :: !Connection
    , logger :: !(LogLevel -> String -> IO ())
    , gldApiKey :: !String
    , gldApiHost :: !String
    , cookieKey :: !Text
    , season :: !Season
    , emailHost :: !String
    , emailUsername :: !String
    , emailPassword :: !String
    }

devEnvFile :: FilePath
devEnvFile = "env/dev.env"

devEnvSecretFile :: FilePath
devEnvSecretFile = "env/dev-secret.env"

prodEnvFile :: FilePath
prodEnvFile = "env/prod.env"

appLogger :: LogLevel -> String -> IO ()
appLogger l s = do
    time <- getCurrentTime
    putStrLn $ show time ++ " :: " ++ show l ++ " :: " ++ s

readAndSet :: FilePath -> IO ()
readAndSet fp = do
    f <- readFile fp
    let ls = lines f
    mapM_ (\e -> 
        case elemIndex '=' e of
            Nothing -> return ()
            Just i -> 
                let (k,v) = splitAt i e
                in setEnv k $ tail v
        ) ls

setEnvVars :: IO ()
setEnvVars = do
    env <- lookupEnv "MAJOR_APP_ENV"
    let runTimeEnv = case env of
            Nothing -> DEV
            Just e -> read e :: AppEnv
    case runTimeEnv of
        DEV -> do
            _ <- readAndSet devEnvFile
            readAndSet devEnvSecretFile
        PROD -> readAndSet prodEnvFile

getAppEnv :: IO Env
getAppEnv = do
    _ <- setEnvVars
    dbHost <- getEnv "POSTGRES_HOST"
    print $ "POSTGRES_HOST: " ++ dbHost ++ "<-"
    dbPort <- getEnv "POSTGRES_PORT"
    print $ "POSTGRES_PORT: " ++  dbPort ++ "<-"
    dbUser <- getEnv "POSTGRES_USER"
    print $ "POSTGRES_USER: " ++  dbUser ++ "<-"
    dbPass <- getEnv "POSTGRES_PW"
    print $ "POSTGRES_PW SIZE: " ++ show (take 4 dbPass)
    apiHost <- getEnv "GLD_API_HOST"
    print $ "GLD_API_HOST: " ++  apiHost ++ "<-"
    apiKey <- getEnv "GLD_API_KEY"
    print $ "GLD_API_KEY: " ++ show (take 4 apiKey)
    season <- getEnv "SEASON"
    print $ "SEASON: " ++  season ++ "<-"
    emailHost <- getEnv "EMAIL_HOST"
    print $ "EMAIL_HOST: " ++  emailHost ++ "<-"
    emailUsername <- getEnv "EMAIL_USERNAME"
    print $ "EMAIL_USERNAME: " ++  emailUsername ++ "<-"
    emailPassword <- getEnv "EMAIL_PASSWORD"
    print $ "EMAIL_PASSWORD SIZE: " ++ show (take 4 emailPassword)
    conn <- R.connect dbHost (read dbPort) dbUser dbPass
    return Env 
        { conn = conn
        , logger = appLogger
        , gldApiKey = apiKey
        , gldApiHost = apiHost
        , cookieKey = "majorplayer"
        , season = read season
        , emailHost = emailHost
        , emailUsername = emailUsername
        , emailPassword = emailPassword
        }
