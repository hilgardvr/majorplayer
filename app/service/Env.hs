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
import System.Directory (doesFileExist)

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

envFile :: FilePath
envFile = ".env"

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
    fx <- doesFileExist envFile
    if fx
    then readAndSet envFile
    else return ()

getAppEnv :: IO Env
getAppEnv = do
    _ <- setEnvVars
    dbHost <- getEnv "POSTGRES_HOST"
    appLogger INFO $ "POSTGRES_HOST: " ++ dbHost ++ "<-"
    dbPort <- getEnv "POSTGRES_PORT"
    appLogger INFO $ "POSTGRES_PORT: " ++  dbPort ++ "<-"
    dbUser <- getEnv "POSTGRES_USER"
    appLogger INFO $ "POSTGRES_USER: " ++  dbUser ++ "<-"
    dbPass <- getEnv "POSTGRES_PW"
    appLogger INFO $ "POSTGRES_PW SIZE: " ++ show (take 4 dbPass)
    apiHost <- getEnv "GLD_API_HOST"
    appLogger INFO $ "GLD_API_HOST: " ++  apiHost ++ "<-"
    apiKey <- getEnv "GLD_API_KEY"
    appLogger INFO $ "GLD_API_KEY: " ++ show (take 4 apiKey)
    season <- getEnv "SEASON"
    appLogger INFO $ "SEASON: " ++  season ++ "<-"
    emailHost <- getEnv "EMAIL_HOST"
    appLogger INFO $ "EMAIL_HOST: " ++  emailHost ++ "<-"
    emailUsername <- getEnv "EMAIL_USERNAME"
    appLogger INFO $ "EMAIL_USERNAME: " ++  emailUsername ++ "<-"
    emailPassword <- getEnv "EMAIL_PASSWORD"
    appLogger INFO $ "EMAIL_PASSWORD SIZE: " ++ show (take 4 emailPassword)
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
