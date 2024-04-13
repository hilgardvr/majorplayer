{-# LANGUAGE OverloadedStrings #-}

module GolfLeaderboardDataApiClient
( getGolferRankings
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), decode)
import Database.PostgreSQL.Simple (ToRow, FromRow, query, query_)
import Data.ByteString.UTF8 (fromString)
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Data.Time (LocalTime, getCurrentTime, utc, utcToLocalTime, diffLocalTime)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Env (Env (conn, logger, gldApiHost, gldApiKey), LogLevel (ERROR, INFO, DEBUG))
import Control.Exception (SomeException, try)
import Network.HTTP.Client.Conduit (Request(method, requestHeaders), parseRequest, Response (responseBody))
import Repo (getQuery)
import Network.HTTP.Simple (httpBS)
import Golfer (Golfer(..))

data ApiMeta = ApiMeta
    { title :: !String
    , description :: !String
    } deriving (Show)

instance FromJSON ApiMeta where
    parseJSON = withObject "ApiMeta" $ \v -> ApiMeta
        <$> v .: "title"
        <*> v .: "description"

data ApiRankings = ApiRankings
    { rankings :: [Golfer]
    } deriving (Show)

instance FromJSON ApiRankings where
    parseJSON = withObject "ApiRankings" $ \v -> ApiRankings
        <$> v .: "rankings"

data RankingApiResponse = RankingApiResponse
    { meta :: !ApiMeta
    , results :: !ApiRankings
    } deriving (Show)

instance FromJSON RankingApiResponse where
    parseJSON = withObject "RankingApiResponse"  $ \v -> RankingApiResponse
        <$> v .: "meta"
        <*> v .: "results"

data RawApiResponse = RawApiResponse 
    { responseId :: !(Maybe UUID)
    , rawResponse :: !String
    , createdAt :: !LocalTime
    }

instance FromJSON Golfer where
    parseJSON = withObject "Golfer"  $ \v -> Golfer
        <$> v .: "player_id"
        <*> v .: "position"
        <*> v .: "player_name"

instance ToRow RawApiResponse where
    toRow (RawApiResponse _ rr _) = [toField rr]

instance FromRow RawApiResponse where
    fromRow = RawApiResponse <$> field <*> field <*> field

getCachedRankingResponse :: Env -> IO (Maybe RawApiResponse)
getCachedRankingResponse env = do
    res <- try $ query_ (conn env) (getQuery "select * from golfer_rankings order by created_at desc limit 1") :: IO (Either SomeException [RawApiResponse])
    case res of
        Left e -> do
            logger env ERROR $ "failed to insert cache" ++ show e
            error "failed to insert cache"
        Right r -> 
            if null r
            then return Nothing
            else do
                nowUtc <- getCurrentTime
                let nowUtcLocal = utcToLocalTime utc nowUtc
                    diff = diffLocalTime nowUtcLocal $ createdAt $ head r
                if diff > 604800
                then return Nothing
                else return $ Just $ head r

getGolferData :: Env -> IO RawApiResponse
getGolferData env = do
    cachedMaybe <- getCachedRankingResponse env 
    case cachedMaybe of
        Nothing -> do
            logger env INFO "no cache received - hitting api"
            initReq <- parseRequest $ gldApiHost env ++ "/world-rankings"
            let req = initReq
                    { method = "GET"
                    , requestHeaders = 
                        [ ("X-RapidAPI-Key", fromString $ gldApiKey env) ]
                    }
            res <- httpBS req
            logger env DEBUG $ "RAW response: " ++ show res
            let body = responseBody res
            dbRes <- try $ query (conn env) (getQuery "insert into golfer_rankings (raw_response) values (?) returning *") [toField body]  :: IO (Either SomeException [RawApiResponse])
            case dbRes of 
                Left e -> do
                    logger env ERROR $ "error inserting golfer rankings: " ++ show e
                    error $ "error inserting golfer rankings: " ++ show e
                Right r -> do
                    logger env DEBUG $ "inserted golfer rankings - createdAt: " ++ (show . createdAt $ head r)
                    return $ head r
        Just c -> do
            logger env DEBUG "found golfer cache"
            return c

getGolferRankings :: Env -> IO [Golfer]
getGolferRankings env = do
    body <- getGolferData env
    let decoded = Data.Aeson.decode $ BSL.fromString $ rawResponse body :: Maybe RankingApiResponse
    print $ take 500 $ rawResponse body
    case decoded of
        Nothing -> do
            logger env ERROR "failed to decode golfers from json"
            error "failded to decode api golfers"
        Just gs -> do
            logger env INFO "parsed json success"
            return $ rankings . results $ gs