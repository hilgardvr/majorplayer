{-# LANGUAGE OverloadedStrings #-}

module GLDApiClient
( getGLDClient
) where
import Data.Aeson (decode)
import Database.PostgreSQL.Simple (ToRow, FromRow, query, query_)
import Data.ByteString.UTF8 (fromString)
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Data.Time (LocalTime, diffLocalTime, NominalDiffTime, addLocalTime)
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
import Fixture (FixtureAPIResponse (results), Fixture (startDate, Fixture, id), FixtureId, NotStartedFixture, StartedFixture)
import qualified GLDApiRankings as RankingApiResponse
import GLDApiLeaderboard (ApiLeaderboardResponse(..), ApiLeaderboard(..), apiToLeaderboardGolfer)
import Leaderboard (LeaderboardGolfer)
import DataClient (DataClientApi(..))
import Data.List (sortBy)
import Utils (getSafeHead, nowUtc, add12h, daySeconds)

data GLDApiClient = GLDApiClient
    { gldRankings :: !(IO [Golfer])
    , gldFixtures :: !(IO [Fixture])
    , gldLeaderboard :: !(FixtureId -> IO [LeaderboardGolfer])
    , gldGetPrePostStartDate :: !(IO (Maybe Fixture, Maybe Fixture))
    , gldRefreshRankings :: !(IO ())
    , gldRefreshLeaderboard :: !(IO ())
    , gldRefreshFixtures :: !(IO ())
    }

instance DataClientApi GLDApiClient where
    getGolferRankings (GLDApiClient rankings fixures leaderboard dates refRankings refLeader refFix) = rankings
    getFixures (GLDApiClient rankings fixtures leaderboard dates refRankings refLeader refFix) = fixtures
    getFixtureLeaderboard (GLDApiClient rankings fixures leaderboard dates refRankings refLeader refFix) = leaderboard
    getPrePostStartDate (GLDApiClient rankings fixures leaderboard dates refRankings refLeader refFix) = dates
    refreshRankings (GLDApiClient rankings fixures leaderboard dates refRankings refLeader refFix) = refRankings
    refreshLeaderboard (GLDApiClient rankings fixures leaderboard dates refRankings refLeader refFix) = refLeader
    refreshFixtures (GLDApiClient rankings fixures leaderboard dates refRankings refLeader refFix) = refFix

getGLDClient :: Env -> GLDApiClient
getGLDClient env =
    GLDApiClient 
        { gldRankings = getGLDGolferRankings env
        , gldFixtures = getGLDFixures env
        , gldLeaderboard = getGLDLeaderboard env
        , gldGetPrePostStartDate = getGLDPrePostStartDate env
        , gldRefreshFixtures = refreshGLDFixtures env
        , gldRefreshRankings = refreshGLDRankings env
        , gldRefreshLeaderboard = refreshGLDLeaderboard env
        }

type TableName = String
type EndPoint = String

rawGolferRankingTable :: TableName
rawGolferRankingTable = "golfer_rankings"

rankingsEndpoint :: EndPoint
rankingsEndpoint = "/world-rankings"

rawFixtureTable :: TableName
rawFixtureTable = "fixtures"

fixturesEndpoint :: Season -> EndPoint
fixturesEndpoint s = "/fixtures/1/" ++ show s

rawLeaderboardTable :: TableName
rawLeaderboardTable = "leaderboard"

leaderboardEndpoint :: FixtureId -> EndPoint
leaderboardEndpoint fid = "/leaderboard/" ++ show fid


getGLDPrePostStartDate :: Env -> IO (Maybe NotStartedFixture, Maybe StartedFixture)
getGLDPrePostStartDate env = do
    fixtures <- getGLDFixures env
    nowUtcLocal <- nowUtc
    let sorted  = sortByStartDate fixtures
        notStarted = getSafeHead $ dropWhile (\e -> nowUtcLocal > (add12h $ startDate e)) sorted
        started = getSafeHead $ dropWhile (\e -> nowUtcLocal < (add12h $ startDate e)) $ reverse sorted
    pure (notStarted, started)

sortByStartDate :: [Fixture] -> [Fixture]
sortByStartDate = sortBy (\x y -> compare (startDate x) (startDate y))

getGLDFixures :: Env -> IO [Fixture]
getGLDFixures env = do
    body <- getRawApiResponse env (fixturesEndpoint (season env)) rawFixtureTable daySeconds
    let decoded = Data.Aeson.decode $ BSL.fromString $ rawResponse body :: Maybe FixtureAPIResponse
    case decoded of
        Nothing -> do
            logger env ERROR "failed to decode fixures from json"
            error "failded to decode api fixtures"
        Just fs -> do
            logger env INFO "parsed json fixures success"
            return $ Fixture.results fs
    

refreshGLDRankings :: Env -> IO ()
refreshGLDRankings env = do
    logger env INFO "START :: refreshed rankings"
    res <- hitApiAndPersist env rankingsEndpoint rawGolferRankingTable
    logger env INFO "END :: refreshed rankings"

refreshGLDLeaderboard :: Env -> IO ()
refreshGLDLeaderboard env = do
    (_, started) <- getGLDPrePostStartDate env
    case started of
        Nothing -> do
            logger env ERROR "no started fixure - not refreshing leaderboard"
            error "no started fixure - not refreshing leaderboard"
        Just s -> do
            logger env INFO "START :: refresh leaderboard"
            res <- hitApiAndPersist env (leaderboardEndpoint $ Fixture.id s) rawLeaderboardTable
            logger env INFO "END :: refreshed leaderboard"

refreshGLDFixtures :: Env -> IO ()
refreshGLDFixtures env = do
    logger env INFO "START :: refresh fixtures"
    res <- hitApiAndPersist env (fixturesEndpoint $ season env) rawFixtureTable
    logger env INFO "END :: refreshed fixtures"

getGLDGolferRankings :: Env -> IO [Golfer]
getGLDGolferRankings env = do
    body <- getRawApiResponse env rankingsEndpoint rawGolferRankingTable daySeconds  -- 24h
    let decoded = Data.Aeson.decode $ BSL.fromString $ rawResponse body :: Maybe RankingApiResponse
    case decoded of
        Nothing -> do
            logger env ERROR "failed to decode golfers rankings from json"
            error "failded to decode api golfers"
        Just gs -> do
            logger env INFO "parsed golfers rankings json success"
            let apiGolfers = rankings . RankingApiResponse.results $ gs
            return $ map toGolfer apiGolfers

getGLDLeaderboard :: Env -> FixtureId -> IO [LeaderboardGolfer]
getGLDLeaderboard env fid = do
    body <- getRawApiResponse env (leaderboardEndpoint fid) rawLeaderboardTable 900 -- 15min
    let decoded = Data.Aeson.decode $ BSL.fromString $ rawResponse body :: Maybe ApiLeaderboardResponse
    case decoded of
        Nothing -> do
            logger env ERROR "failed to decode leaderboard from json"
            error "failded to decode api leaderboard from json"
        Just gs -> do
            logger env INFO "parsed leaderboard json success"
            let apiLeaderboard = GLDApiLeaderboard.leaderboard $ GLDApiLeaderboard.results gs
            return $ map apiToLeaderboardGolfer apiLeaderboard

data RawApiResponse = RawApiResponse 
    { responseId :: !(Maybe UUID)
    , rawResponse :: !String
    , createdAt :: !LocalTime
    }

instance ToRow RawApiResponse where
    toRow (RawApiResponse _ rr _) = [toField rr]

instance FromRow RawApiResponse where
    fromRow = RawApiResponse <$> field <*> field <*> field

getCachedResponse :: Env -> TableName -> IO (Maybe RawApiResponse)
getCachedResponse env tn = do
    res <- try $ query_ (conn env) (getQuery $ "select * from " ++ tn ++ " order by created_at desc limit 1") :: IO (Either SomeException [RawApiResponse])
    case res of
        Left e -> do
            logger env ERROR $ "failed to get cache for " ++ tn ++ " - error: " ++ show e
            error $ "failed to get cache for " ++ tn ++ " - error: " ++ show e
        Right r -> 
            if null r
            then return Nothing
            else return $ Just $ head r

hitApiAndPersist :: Env -> EndPoint -> TableName -> IO RawApiResponse
hitApiAndPersist env endpoint table = do
    logger env INFO ("hitting api " ++ endpoint)
    initReq <- parseRequest $ gldApiHost env ++ endpoint
    let req = initReq
            { method = "GET"
            , requestHeaders = 
                [ ("X-RapidAPI-Key", fromString $ gldApiKey env) ]
            }
    res <- httpBS req
    logger env DEBUG $ " RAW response form endpoint: " ++ endpoint ++ "- Raw: " ++  take 200 (show res)
    let body = responseBody res
    dbRes <- try $ query (conn env) (getQuery $ "insert into " ++ table ++ " (raw_response) values (?) returning *") [toField body]  :: IO (Either SomeException [RawApiResponse])
    case dbRes of 
        Left e -> do
            logger env ERROR $ "error inserting into " ++ table ++ ": " ++ show e
            error $ "error inserting into " ++ table ++ ": " ++ show e
        Right r -> do
            logger env DEBUG $ "inserted into " ++ table ++ " - createdAt: " ++ (show . createdAt $ head r)
            return $ head r

getRawApiResponse :: Env -> EndPoint -> TableName -> NominalDiffTime -> IO RawApiResponse
getRawApiResponse env endpoint table _ = do
    cachedMaybe <- getCachedResponse env table
    case cachedMaybe of
        Nothing -> hitApiAndPersist env endpoint table
        Just c -> pure c
