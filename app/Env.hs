module Env
( LogLevel(..)
, Env(..)
, getAppEnv
) where
import Database.PostgreSQL.Simple (Connection)
import Data.Time (getCurrentTime)
import System.Environment (getEnv, setEnv, lookupEnv)
import Data.List (elemIndex)
import qualified Repo as R (connect)
--import GHC.Stack ( HasCallStack, getCallStack, callStack )

data LogLevel = DEBUG | INFO | WARN | ERROR deriving (Show)

data AppEnv = DEV | PROD deriving (Read, Show)

data Env = Env 
    { conn :: !Connection
    , logger :: !(LogLevel -> String -> IO ())
    , gldApiKey :: !String
    , gldApiHost :: !String
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

splitAtUnSafe :: Char -> String -> (String, String)
splitAtUnSafe c s = (takeWhile (/= c) s, dropWhile (/= c) s)

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
    print dbHost
    dbPort <- getEnv "POSTGRES_PORT"
    print dbPort
    dbUser <- getEnv "POSTGRES_USER"
    print dbUser
    dbPass <- getEnv "POSTGRES_PW"
    print dbPass
    apiHost <- getEnv "GLD_API_HOST"
    print apiHost
    apiKey <- getEnv "GLD_API_KEY"
    print apiKey
    conn <- R.connect dbHost (5432) dbUser dbPass
    return Env 
        { conn = conn
        , logger = appLogger
        , gldApiKey = apiKey
        , gldApiHost = apiHost
        }
