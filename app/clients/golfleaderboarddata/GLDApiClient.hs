{-# LANGUAGE OverloadedStrings #-}

module GLDApiClient
( getGolferRankings
, getFixures
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
import Env (Env (conn, logger, gldApiHost, gldApiKey, season), LogLevel (ERROR, INFO, DEBUG), Season)
import Control.Exception (SomeException, try)
import Network.HTTP.Client.Conduit (Request(method, requestHeaders), parseRequest, Response (responseBody))
import Repo (getQuery)
import Network.HTTP.Simple (httpBS)
import Golfer (Golfer(..))
import GLDApiRankings (RankingApiResponse, ApiRankings (rankings))
import GLDApiGolfer (toGolfer)
import Fixture (FixtureAPIResponse (results), Fixture, FixtureId)
import qualified GLDApiRankings as RankingApiResponse
import GLDApiLeaderboard (ApiLeaderboardResponse(ApiLeaderboardResponse))

type TableName = String
type EndPoint = String

rawGolferRankingTable :: TableName
rawGolferRankingTable = "golfer_rankings"

rankingsEndpoint :: EndPoint
rankingsEndpoint = "/world-rankings"

rawFixtureTable :: TableName
rawFixtureTable = "fixtures"

fixturesEndpoint :: Season -> EndPoint
fixturesEndpoint s = "/fixtures/2/" ++ show s

rawLeaderboardTable :: TableName
rawLeaderboardTable = "leaderboard"

leaderboardEndpoint :: FixtureId -> EndPoint
leaderboardEndpoint fid = "/leaderboard/" ++ show fid

getFixures :: Env -> IO [Fixture]
getFixures env = do
    body <- getRawApiResponse env (fixturesEndpoint (season env)) rawFixtureTable 604800 
    let decoded = Data.Aeson.decode $ BSL.fromString $ rawResponse body :: Maybe FixtureAPIResponse
    case decoded of
        Nothing -> do
            logger env ERROR "failed to decode fixures from json"
            error "failded to decode api fixtures"
        Just fs -> do
            logger env INFO "parsed json fixures success"
            return $ Fixture.results fs
    

getGolferRankings :: Env -> IO [Golfer]
getGolferRankings env = do
    --body <- getGolferApiData env
    body <- getRawApiResponse env rankingsEndpoint rawGolferRankingTable 604800 
    let decoded = Data.Aeson.decode $ BSL.fromString $ rawResponse body :: Maybe RankingApiResponse
    --print $ take 500 $ rawResponse body
    case decoded of
        Nothing -> do
            logger env ERROR "failed to decode golfers from json"
            error "failded to decode api golfers"
        Just gs -> do
            logger env INFO "parsed json success"
            let apiGolfers = rankings . RankingApiResponse.results $ gs
            return $ map toGolfer apiGolfers

getLeaderboard :: Env -> FixtureId -> IO ()
getLeaderboard env fid = do
    --body <- getGolferApiData env
    body <- getRawApiResponse env (leaderboardEndpoint fid) rawLeaderboardTable 900 
    let decoded = Data.Aeson.decode $ BSL.fromString $ rawResponse body :: Maybe ApiLeaderboardResponse
    --print $ take 500 $ rawResponse body
    case decoded of
        Nothing -> do
            logger env ERROR "failed to decode golfers from json"
            error "failded to decode api golfers"
        Just gs -> do
            logger env INFO "parsed json success"

data RawApiResponse = RawApiResponse 
    { responseId :: !(Maybe UUID)
    , rawResponse :: !String
    , createdAt :: !LocalTime
    }

instance ToRow RawApiResponse where
    toRow (RawApiResponse _ rr _) = [toField rr]

instance FromRow RawApiResponse where
    fromRow = RawApiResponse <$> field <*> field <*> field

getCachedResponse :: Env -> TableName -> NominalDiffTime -> IO (Maybe RawApiResponse)
getCachedResponse env tn timeout = do
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

--getGolferApiData :: Env -> IO RawApiResponse
--getGolferApiData env = do
--    cachedMaybe <- getCachedResponse env rawGolferRankingTable 604800
--    case cachedMaybe of
--        Nothing -> do
--            logger env INFO "no cache received - hitting api"
--            initReq <- parseRequest $ gldApiHost env ++ "/world-rankings"
--            let req = initReq
--                    { method = "GET"
--                    , requestHeaders = 
--                        [ ("X-RapidAPI-Key", fromString $ gldApiKey env) ]
--                    }
--            res <- httpBS req
--            logger env DEBUG $ "RAW response: " ++ show res
--            let body = responseBody res
--            dbRes <- try $ query (conn env) (getQuery $ "insert into " ++ rawGolferRankingTable ++ " (raw_response) values (?) returning *") [toField body]  :: IO (Either SomeException [RawApiResponse])
--            case dbRes of 
--                Left e -> do
--                    logger env ERROR $ "error inserting golfer rankings: " ++ show e
--                    error $ "error inserting golfer rankings: " ++ show e
--                Right r -> do
--                    logger env DEBUG $ "inserted golfer rankings - createdAt: " ++ (show . createdAt $ head r)
--                    return $ head r
--        Just c -> do
--            logger env DEBUG "found golfer cache"
--            return c
--
--getFixtureApiData :: Env -> IO RawApiResponse
--getFixtureApiData env = do
--    cachedMaybe <- getCachedResponse env rawFixtureTable 6048000
--    case cachedMaybe of
--        Nothing -> do
--            logger env INFO "no cache received - hitting api"
--            initReq <- parseRequest $ gldApiHost env ++ "/fixtures/2/" ++ show (season env)
--            let req = initReq
--                    { method = "GET"
--                    , requestHeaders = 
--                        [ ("X-RapidAPI-Key", fromString $ gldApiKey env) ]
--                    }
--            res <- httpBS req
--            logger env DEBUG $ "RAW response: " ++ show res
--            let body = responseBody res
--            dbRes <- try $ query (conn env) (getQuery $ "insert into " ++ rawFixtureTable ++ " (raw_response) values (?) returning *") [toField body]  :: IO (Either SomeException [RawApiResponse])
--            case dbRes of 
--                Left e -> do
--                    logger env ERROR $ "error inserting fixtures: " ++ show e
--                    error $ "error fixtures golfer rankings: " ++ show e
--                Right r -> do
--                    logger env DEBUG $ "inserted fixtures - createdAt: " ++ (show . createdAt $ head r)
--                    return $ head r
--        Just c -> do
--            logger env DEBUG "found fixures cache"
--            return c

getRawApiResponse :: Env -> EndPoint -> TableName -> NominalDiffTime -> IO RawApiResponse
getRawApiResponse env endpoint table cacheTimeout = do
    cachedMaybe <- getCachedResponse env table cacheTimeout
    case cachedMaybe of
        Nothing -> do
            logger env INFO ("no cache received - hitting api " ++ endpoint)
            initReq <- parseRequest $ gldApiHost env ++ endpoint
            let req = initReq
                    { method = "GET"
                    , requestHeaders = 
                        [ ("X-RapidAPI-Key", fromString $ gldApiKey env) ]
                    }
            res <- httpBS req
            logger env DEBUG $ endpoint ++ " RAW response: " ++ show res
            let body = responseBody res
            dbRes <- try $ query (conn env) (getQuery $ "insert into " ++ table ++ " (raw_response) values (?) returning *") [toField body]  :: IO (Either SomeException [RawApiResponse])
            case dbRes of 
                Left e -> do
                    logger env ERROR $ "error inserting into " ++ table ++ ": " ++ show e
                    error $ "error inserting into " ++ table ++ ": " ++ show e
                Right r -> do
                    logger env DEBUG $ "inserted into " ++ table ++ " - createdAt: " ++ (show . createdAt $ head r)
                    return $ head r
        Just c -> do
            logger env DEBUG "found fixures cache"
            return c
    

--getLeaderboardApiData :: Env -> FixtureId -> IO RawApiResponse
--getLeaderboardApiData env fid = do
--    cachedMaybe <- getCachedResponse env rawLeaderboardTable 900
--    case cachedMaybe of
--        Nothing -> do
--            logger env INFO "no cache received - hitting api"
--            initReq <- parseRequest $ gldApiHost env ++ "/fixtures/leaderboard/" ++ show fid
--            let req = initReq
--                    { method = "GET"
--                    , requestHeaders = 
--                        [ ("X-RapidAPI-Key", fromString $ gldApiKey env) ]
--                    }
--            res <- httpBS req
--            logger env DEBUG $ "RAW response: " ++ show res
--            let body = responseBody res
--            dbRes <- try $ query (conn env) (getQuery $ "insert into " ++ rawLeaderboardTable ++ " (raw_response) values (?) returning *") [toField body]  :: IO (Either SomeException [RawApiResponse])
--            case dbRes of 
--                Left e -> do
--                    logger env ERROR $ "error inserting leaderboard: " ++ show e
--                    error $ "error inserting leaderboard: " ++ show e
--                Right r -> do
--                    logger env DEBUG $ "inserted leaderboard - createdAt: " ++ (show . createdAt $ head r)
--                    return $ head r
--        Just c -> do
--            logger env DEBUG "found fixures cache"
--            return c

