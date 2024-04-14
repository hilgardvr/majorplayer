{-# LANGUAGE OverloadedStrings #-}

module GLDApiClient
( getGolferRankings
) where
import Data.Aeson (decode)
import Database.PostgreSQL.Simple (ToRow, FromRow, query, query_)
import Data.ByteString.UTF8 (fromString)
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Data.Time (LocalTime, getCurrentTime, utc, utcToLocalTime, diffLocalTime, NominalDiffTime)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Env (Env (conn, logger, gldApiHost, gldApiKey), LogLevel (ERROR, INFO, DEBUG))
import Control.Exception (SomeException, try)
import Network.HTTP.Client.Conduit (Request(method, requestHeaders), parseRequest, Response (responseBody))
import Repo (getQuery)
import Network.HTTP.Simple (httpBS)
import Golfer (Golfer(..))
import GLDApiRankings (RankingApiResponse (results), ApiRankings (rankings))
import GLDApiGolfer (toGolfer)

type TableName = String

rawGolferRankingTable :: TableName
rawGolferRankingTable = "golfer_rankings"

data RawApiResponse = RawApiResponse 
    { responseId :: !(Maybe UUID)
    , rawResponse :: !String
    , createdAt :: !LocalTime
    }

instance ToRow RawApiResponse where
    toRow (RawApiResponse _ rr _) = [toField rr]

instance FromRow RawApiResponse where
    fromRow = RawApiResponse <$> field <*> field <*> field

getCachedRankingResponse :: Env -> TableName -> NominalDiffTime -> IO (Maybe RawApiResponse)
getCachedRankingResponse env tn timeout = do
    res <- try $ query_ (conn env) (getQuery $ "select * from " ++ tn ++ " order by created_at desc limit 1") :: IO (Either SomeException [RawApiResponse])
    case res of
        Left e -> do
            logger env ERROR $ "failed to get cache for " ++ tn ++ " - error: " ++ show e
            error $ "failed to get cache for " ++ tn ++ " - error: " ++ show e
        Right r -> 
            if null r
            then return Nothing
            else do
                nowUtc <- getCurrentTime
                let nowUtcLocal = utcToLocalTime utc nowUtc
                    diff = diffLocalTime nowUtcLocal $ createdAt $ head r
                if diff > timeout
                then return Nothing
                else return $ Just $ head r

getGolferApiData :: Env -> IO RawApiResponse
getGolferApiData env = do
    cachedMaybe <- getCachedRankingResponse env rawGolferRankingTable 604800
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
            dbRes <- try $ query (conn env) (getQuery $ "insert into " ++ rawGolferRankingTable ++ " (raw_response) values (?) returning *") [toField body]  :: IO (Either SomeException [RawApiResponse])
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
    body <- getGolferApiData env
    let decoded = Data.Aeson.decode $ BSL.fromString $ rawResponse body :: Maybe RankingApiResponse
    print $ take 500 $ rawResponse body
    case decoded of
        Nothing -> do
            logger env ERROR "failed to decode golfers from json"
            error "failded to decode api golfers"
        Just gs -> do
            logger env INFO "parsed json success"
            let apiGolfers = rankings . results $ gs
            return $ map toGolfer apiGolfers
