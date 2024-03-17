module Env
( LogLevel(..)
, Env(..)
, getEnv
) where
import Database.PostgreSQL.Simple (Connection)
import Data.Time (getCurrentTime)

data LogLevel = DEBUG | INFO | WARN | ERROR deriving (Show)

appLogger :: LogLevel -> String -> IO ()
appLogger l s = do
    time <- getCurrentTime --(utcToLocalTime utc time)
    putStrLn $ show time ++ " :: " ++ show l ++ " :: " ++ s

data Env = Env 
    { conn :: !Connection
    , logger :: !(LogLevel -> String -> IO ())
    }

getEnv :: Connection -> Env
getEnv conn = Env conn appLogger
