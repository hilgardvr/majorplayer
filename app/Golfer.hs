{-# LANGUAGE OverloadedStrings #-}

module Golfer
( Golfer(..)
, Ranking
, GolferName
, GolferId
, getGolfers
, getGolferApi
, filterGolfersById
) where 
import Data.List (elemIndex, partition)
import Text.Mustache (ToMustache (toMustache), object, (~>))
import Network.HTTP.Client.Conduit (parseRequest, Request (method, requestHeaders), bodyReaderSource, Response (responseBody))
import Network.HTTP.Simple (httpBS)
import Env (Env (logger, gldApiHost, gldApiKey), LogLevel (DEBUG, ERROR, INFO))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), decode, ToJSON)
import Data.Aeson.Decoding.ByteString (bsToTokens)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Database.PostgreSQL.Simple.Newtypes (Aeson(Aeson))
import qualified Database.PostgreSQL.Simple.Newtypes as Data
import qualified Data.Text.Lazy as BSL
import Data.ByteString.UTF8 (fromString)

type Ranking = Int
type GolferName = String
type GolferId = Int

data Golfer = Golfer
    { id :: !GolferId
    , ranking :: !Ranking
    , name :: !GolferName
    } deriving (Show)

instance ToMustache Golfer where
    toMustache (Golfer { Golfer.id = i, Golfer.ranking = r, Golfer.name = n }) =
        object 
            [ "id" ~> i
            , "ranking" ~> r
            , "name" ~> n
            ]

instance FromJSON Golfer where
    parseJSON = withObject "Golfer"  $ \v ->  Golfer
        <$> v .: "player_id"
        <*> v .: "position"
        <*> v .: "player_name"

wordsSeperated :: Char -> String -> [String]
wordsSeperated _ [] = []
wordsSeperated c xs = case takeWhile (/=c) xs of
    [] -> []
    s' ->
        let tl = dropWhile (/= c) xs
        in
            if null tl
            then [s']
            else s' : wordsSeperated c (tail tl)

cleanString :: String -> String
cleanString [] = []
cleanString (h:t) = if h == '"' then cleanString t else h : cleanString t

seperateAndClean :: Char -> String -> [String]
seperateAndClean c s =
    let sep = wordsSeperated c s
    in map cleanString sep

filterGolfersById :: [GolferId] -> [Golfer] -> ([Golfer], [Golfer])
filterGolfersById gids = partition (\e -> Golfer.id e `elem` gids)

data ApiResponse = ApiResponse
    { results :: ApiRankings
    } deriving (Show)

instance FromJSON ApiResponse where
    parseJSON = withObject "ApiResponse" $ \v -> ApiResponse
        <$> v .: "results"

data ApiRankings = ApiRankings
    { rankings :: [Golfer]
    } deriving (Show)

instance FromJSON ApiRankings where
    parseJSON = withObject "ApiRankings" $ \v -> ApiRankings
        <$> v .: "rankings"


getGolferApi :: Env -> IO [Golfer]
getGolferApi env = do
    --initReq <- parseRequest "https://golf-leaderboard-data.p.rapidapi.com/world-rankings"
    --let req = initReq
    --        { method = "GET"
    --        , requestHeaders = 
    --            [ ("X-RapidAPI-Key", fromString $ gldApiKey env) ]
    --        }
    --res <- httpBS req
    --logger env DEBUG $ "RAW response: " ++ show res
    --let body = show $ responseBody res
    --writeFile "apiout.json" body
    --logger env DEBUG (gldApiHost env ++ " - " ++ gldApiKey env)
    body <- readFile "apiout.json"
    --writeFile "apiout.json" body
    logger env DEBUG $ "Response body: " ++ body
    putStrLn body
    let decoded = Data.Aeson.decode $ BSL.fromString body :: Maybe ApiResponse
    case decoded of
        Nothing -> do
            logger env ERROR "Failed to decode golfers from json"
            error "Failded to decode api golfers"
        Just gs -> do
            logger env INFO $ "Parsed json: " ++ show gs
            print decoded
            --let tokens = bsToTokens body
            return $ rankings . results $ gs

getGolfers :: IO [Golfer]
getGolfers = do
    f <- readFile "downloaded_rankings.csv"
    ls <- return $ take 200 $ lines f
    sep <- return $ wordsSeperated ',' (head ls)
    let nameIndex = case elemIndex "\"NAME\"" sep of
            Nothing -> error "Name not found"
            Just i -> i
        rankingIndex = case elemIndex "\"RANKING\"" sep of
            Nothing -> error "Ranking not found"
            Just i -> i
        playerColumn = 0 --case elemIndex "\"Player Id\"" (wordsSeperated ',' (head ls)) of
            --Nothing -> error "Ranking not found"
            --Just i -> i
        ps = map (\e -> 
            let s = seperateAndClean ',' e in
                Golfer (read (s!!playerColumn) :: GolferId) (read (s!!rankingIndex) :: Ranking) (s!!nameIndex)
            ) (tail ls)
    return ps
