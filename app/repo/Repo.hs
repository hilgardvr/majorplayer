{-# LANGUAGE OverloadedStrings #-}

module Repo
( getQuery
, connect
, DBHOST
, DBPORT
, DBUSER
, DBPASS
) where
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, execute_, Query)
import Data.ByteString.UTF8 as BSU
import System.Directory (getDirectoryContents)
import Data.Text as T
import Data.Foldable (traverse_)
import Data.Char (isDigit)
import Data.List (sortBy)
import qualified Data.String

type DBHOST = String
type DBPORT = Int
type DBUSER = String
type DBPASS = String

migrationsDir = "./migrations"

getQuery :: String -> Query
getQuery = Data.String.fromString

migrate :: Connection -> IO ()
migrate conn = do
    files <- getDirectoryContents migrationsDir
    let migFiles = Prelude.filter (\e -> 
            let spl = Prelude.filter (/="") (T.split (=='.') $ T.pack e) 
            in Prelude.length spl == 3 && spl!!2 == "sql" && Prelude.all isDigit (T.unpack $ spl!!0)) files
    let sorted = sortBy (\x y ->
            let x' = read (Prelude.takeWhile (/='.') x) :: Int
                y' = read (Prelude.takeWhile (/='.') y) :: Int
            in compare x' y') migFiles
    print $ "sorted: " ++ show sorted
    traverse_ (\f -> do
            _ <- print ("running: " ++ show f)
            contents <- readFile $ migrationsDir ++ "/" ++ f
            res <- execute_ conn $ getQuery contents
            _ <- print (show f ++ ": " ++ show res)
            return ()
        ) sorted

connect :: DBHOST -> DBPORT -> DBUSER -> DBPASS -> IO Connection
connect host port user pass = do
    let connString = "host=" ++ host ++ " port=" ++ show port ++ " user=" ++ user ++ " password=" ++ pass
    conn <- connectPostgreSQL $ BSU.fromString connString
    migrate conn
    return conn
