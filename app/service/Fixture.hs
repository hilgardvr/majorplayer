{-# LANGUAGE OverloadedStrings #-}

module Fixture
( 
) where 
import Network.HTTP.Client.Conduit (parseRequest, Request (method, requestHeaders), Response (responseBody), defaultPath)
import Network.HTTP.Simple (httpBS)
import Env (Env (logger, gldApiHost, gldApiKey, conn, season), LogLevel (DEBUG, ERROR, INFO, WARN))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), decode, object)
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Data.Time (LocalTime, getCurrentTime, utcToLocalTime, utc, diffLocalTime)
import Database.PostgreSQL.Simple (ToRow, FromRow, query, query_)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Control.Exception (SomeException, try)
import Repo (getQuery)
import Database.PostgreSQL.Simple.Newtypes (Aeson(Aeson))
import Data.UUID (UUID)

type FixtureId = Int
type TourId = Int
type Season = Int
--data FixtureAPIResponse = FixtureAPIResponse
--    { meta :: ApiMeta
--    , results :: [Fixture]
--    }
--
data Fixture = Fixture
    { id :: !FixtureId
    , fixureType :: !String
    , status :: !String
    , name :: !String
    , tourId :: !TourId
    , country :: !String
    , course :: !String
    , startDate :: !LocalTime
    , endDate :: !LocalTime
    , season :: !Season
    , timezone :: !String
    , prize :: !String
    , prizeCurrency :: !String
    , updatedAt :: !LocalTime
    } deriving (Show)


instance FromJSON Fixture where
    parseJSON = withObject "Fixture" $ \v ->  Fixture
        <$> v .: "id"
        <*> v .: "type"
        <*> v .: "status"
        <*> v .: "name"
        <*> v .: "tour_id"
        <*> v .: "country"
        <*> v .: "course"
        <*> v .: "start_date"
        <*> v .: "end_date"
        <*> v .: "season"
        <*> v .: "timezone"
        <*> v .: "prize_fund"
        <*> v .: "fund_currency"
        <*> v .: "updated"

--instance FromJSON FixtureAPIResponse where
--    parseJSON = withObject "ApiResponse [Fixture]" $ \v -> FixtureAPIResponse
--        <$> v .: "meta"
--        <*> v .: "results"
--
--data RawFixtureApiResponse = RawFixtureApiResponse 
--    { responseId :: !(Maybe UUID)
--    , rawResponse :: !String
--    , createdAt :: !LocalTime
--    }
--
--instance ToRow RawFixtureApiResponse where
--    toRow (RawFixtureApiResponse _ rr _) = [toField rr]
--
--instance FromRow RawFixtureApiResponse where
--    fromRow = RawFixtureApiResponse <$> field <*> field <*> field


--getCachedFixtureResponse :: Env -> IO (Maybe RawFixtureApiResponse)
--getCachedFixtureResponse env = do
--    res <- try $ query_ (conn env) (getQuery "select * from fixures order by created_at desc limit 1") :: IO (Either SomeException [RawFixtureApiResponse])
--    case res of
--        Left e -> do
--            logger env ERROR $ "failed to get fixture cache cache" ++ show e
--            error "failed to get fixture cache"
--        Right r -> 
--            if null r
--            then return Nothing
--            else do
--                nowUtc <- getCurrentTime
--                let nowUtcLocal = utcToLocalTime utc nowUtc
--                    diff = diffLocalTime nowUtcLocal $ createdAt $ head r
--                if diff > 604800
--                then do
--                    logger env WARN $ "Expired fixture cached found but ignored. Time diff: " ++ show diff
--                    return Nothing
--                else return $ Just $ head r
--
--getFixtureApi :: Env -> IO RawFixtureApiResponse
--getFixtureApi env = do
--    cachedMaybe <- getCachedFixtureResponse env 
--    case cachedMaybe of
--        Nothing -> do
--            logger env INFO "no cache received - hitting api"
--            initReq <- parseRequest $ gldApiHost env ++ "/fixures/2/" ++ (show $ Env.season env)
--            let req = initReq
--                    { method = "GET"
--                    , requestHeaders = 
--                        [ ("X-RapidAPI-Key", fromString $ gldApiKey env) ]
--                    }
--            res <- httpBS req
--            logger env DEBUG $ "RAW response: " ++ show res
--
--            let body = responseBody res
--            dbRes <- try $ query (conn env) (getQuery "insert into fixtures (raw_response) values (?) returning *") [toField body]  :: IO (Either SomeException [RawFixtureApiResponse])
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
--getFixtureData :: Env -> IO [Fixture]
--getFixtureData env = do
--    body <- getFixtureApi env
--    let decoded = Data.Aeson.decode $ BSL.fromString $ rawResponse body :: Maybe FixtureAPIResponse
--    print $ take 500 $ rawResponse body
--    case decoded of
--        Nothing -> do
--            logger env ERROR "failed to decode golfers from json"
--            error "failded to decode api golfers"
--        Just fs -> do
--            logger env INFO "parsed json success"
--            return $ results fs

