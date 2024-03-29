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
import Env (Env (logger), LogLevel (DEBUG, ERROR, INFO))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), decode, ToJSON)
import Data.Aeson.Decoding.ByteString (bsToTokens)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Database.PostgreSQL.Simple.Newtypes (Aeson(Aeson))
import qualified Database.PostgreSQL.Simple.Newtypes as Data
import qualified Data.Text.Lazy as BSL

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
    --            [ ("X-RapidAPI-Key", "30a5faa502msh556ae9557a9e7f6p1370dajsn3dc340c98379") ]
    --        }
    --res <- httpBS req
    --logger env DEBUG $ "RAW response: " ++ show res
    --let body = show $ responseBody res
    let body = temp
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

--temp = "[{\"position\":149,\"movement\":6,\"player_id\":136843,\"player_name\":\"Tyler Duncan\",\"num_events\":52,\"avg_points\":\"0.7894\",\"total_points\":\"41.0489\",\"points_lost\":\"-8.2289\",\"points_gained\":\"5.9441\"},{\"position\":150,\"movement\":9,\"player_id\":164365,\"player_name\":\"Dylan Wu\",\"num_events\":52,\"avg_points\":\"0.7813\",\"total_points\":\"40.6289\",\"points_lost\":\"-6.1952\",\"points_gained\":\"11.5021\"},{\"position\":151,\"movement\":13,\"player_id\":156007,\"player_name\":\"Andrew Novak\",\"num_events\":52,\"avg_points\":\"0.7737\",\"total_points\":\"40.2315\",\"points_lost\":\"-5.1368\",\"points_gained\":\"21.9121\"},{\"position\":152,\"movement\":10,\"player_id\":138121,\"player_name\":\"Callum Shinkwin\",\"num_events\":44,\"avg_points\":\"0.7734\",\"total_points\":\"34.0292\",\"points_lost\":\"-9.6452\",\"points_gained\":\"8.4032\"},{\"position\":153,\"movement\":6,\"player_id\":111214,\"player_name\":\"Grant Forrest\",\"num_events\":52,\"avg_points\":\"0.7697\",\"total_points\":\"40.026\",\"points_lost\":\"-8.1873\",\"points_gained\":\"2.3657\"},{\"position\":154,\"movement\":3,\"player_id\":83824,\"player_name\":\"Martin Laird\",\"num_events\":49,\"avg_points\":\"0.7652\",\"total_points\":\"37.4936\",\"points_lost\":\"-7.9545\",\"points_gained\":\"11.0204\"},{\"position\":155,\"movement\":7,\"player_id\":103243,\"player_name\":\"David Lingmerth\",\"num_events\":49,\"avg_points\":\"0.7647\",\"total_points\":\"37.4688\",\"points_lost\":\"-10.6710\",\"points_gained\":\".0000\"},{\"position\":156,\"movement\":6,\"player_id\":5446,\"player_name\":\"Phil Mickelson\",\"num_events\":40,\"avg_points\":\"0.7594\",\"total_points\":\"30.375\",\"points_lost\":\"-7.0312\",\"points_gained\":\".0000\"},{\"posi tion\":157,\"movement\":4,\"player_id\":83326,\"player_name\":\"Camilo Villegas\",\"num_events\":52,\"avg_points\":\"0.7513\",\"total_points\":\"39.069\",\"points_lost\":\"-4.2637\",\"points_gained\":\"1.9788\"},{\"position\":158,\"movement\":9,\"player_id\":142603,\"play er_name\":\"Mito Pereira\",\"num_events\":40,\"avg_points\":\"0.7501\",\"total_points\":\"30.0025\",\"points_lost\":\"-14.5329\",\"points_gained\":\"5.3818\"},{\"position\":159,\"movement\":8,\"player_id\":103705,\"player_name\":\"Chesson Hadley\",\"num_events\":51,\"avg_ points\":\"0.7485\",\"total_points\":\"38.1725\",\"points_lost\":\"-7.8521\",\"points_gained\":\"6.9940\"},{\"position\":160,\"movement\":5,\"player_id\":154816,\"player_name\":\"Jimmy Stanger\",\"num_events\":49,\"avg_points\":\"0.7458\",\"total_points\":\"36.5464\",\"po ints_lost\":\"-5.2948\",\"points_gained\":\"17.5706\"},{\"position\":161,\"movement\":4,\"player_id\":157126,\"player_name\":\"Mason Andersen\",\"num_events\":40,\"avg_points\":\"0.745\",\"total_points\":\"29.7988\",\"points_lost\":\"-2.5651\",\"points_gained\":\"15.4264\"}]"
temp :: String
temp = "{\"results\":{\"rankings\": [{\"position\":149,\"movement\":6,\"player_id\":136843,\"player_name\":\"Tyler Duncan\",\"num_events\":52,\"avg_points\":\"0.7894\",\"total_points\":\"41.0489\",\"points_lost\":\"-8.2289\",\"points_gained\":\"5.9441\"}]}}"

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
