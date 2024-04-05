{-# LANGUAGE OverloadedStrings #-}

module League
( League(..)
, createLeague
, getLeaguesForUser
, joinLeague
, getUserIdsForLeague
)
where
import Data.UUID (UUID)
import Env (Env (logger, conn), LogLevel (DEBUG, ERROR))
import Database.PostgreSQL.Simple (ToRow, FromRow, returning, query)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Repo (getQuery)
import User (UserId)
import Text.Mustache (object, ToMustache (toMustache), (~>))
import Utils (getSafeHead)
import Text.StringRandom (stringRandomIO)
import Data.Text (unpack)
import Control.Exception (try, SomeException)

type LeagueId = UUID
type LeagueName = String
type LeaguePasscode = String

data League = League 
    { id :: !(Maybe LeagueId)
    , adminId :: !UserId
    , name :: !LeagueName
    , passcode :: !LeaguePasscode
    } deriving (Show)

instance ToRow League where
    toRow (League _ aid n pc) = [toField aid, toField n, toField pc]

instance FromRow League where
    fromRow = League <$> field <*> field <*> field <*> field

instance ToMustache League where
    toMustache (League i aid n pc) = object
        [ "id" ~> maybe "" show i
        , "adminId" ~> aid
        , "name" ~> n
        , "passcode" ~> pc
        ]

data LeagueUser = LeagueUser
    { leagueId :: !(LeagueId)
    , userId :: !(UserId)
    } deriving (Show)

instance ToRow LeagueUser where
    toRow (LeagueUser lid uid) = [toField lid, toField uid]

instance FromRow LeagueUser where
    fromRow = LeagueUser <$> field <*> field

createRandomPasscode :: IO String
createRandomPasscode = do
    str <- stringRandomIO "^[a-zA-Z0-9]{7}$"
    return $ unpack str

createLeague :: Env -> UserId -> LeagueName -> IO League
createLeague env uid ln = do
    logger env DEBUG $ "creating league " ++ ln
    pc <- createRandomPasscode
    let league = League Nothing uid ln pc
    leagueRows <- returning (conn env) (getQuery "insert into leagues(admin_id, name, passcode) values (?, ?, ?) returning *") [league]
    logger env DEBUG $ "created league "  ++ name league ++ " - expected length 1, got " ++ (show $ length leagueRows)
    if length leagueRows /= 1
    then do
        let msg = "did not get the expected league length of 1"
        logger env ERROR $ msg
        error msg
    else 
        case League.id $ head leagueRows of 
            Nothing -> do
                let msg = "Expected to get a league id"
                logger env ERROR msg
                error msg
            Just lid -> do
                let leagueRow = head leagueRows :: League
                    leagueUser = LeagueUser lid (adminId leagueRow)
                logger env DEBUG $ "creating league user for admin in league: " ++ name leagueRow
                leagueUserRow <- returning (conn env) (getQuery "insert into league_users(league_id, user_id) values (?, ?) returning *") [leagueUser] :: IO [LeagueUser]
                logger env DEBUG $ "created league user for admin in league: " ++ name leagueRow
                if length leagueUserRow /= 1
                then do
                    let msg = "did not get the expected league user length of 1"
                    logger env ERROR msg
                    error msg
                else return leagueRow

getLeaguesForUser :: Env -> UserId -> IO [League]
getLeaguesForUser env uid = do
    logger env DEBUG $ "getting leagues for user: " ++ show uid
    leagues <- query (conn env) (getQuery "select id, admin_id, name, passcode from leagues l join league_users lu on l.id = lu.league_id where lu.user_id = (?)") [uid]
    logger env DEBUG $ "got leagues for user: " ++ show uid ++ " - length " ++ (show $ length leagues)
    return leagues

getUserIdsForLeague :: Env -> LeagueId -> IO [UUID]
getUserIdsForLeague env lid = do
    logger env DEBUG $ "getting users for league: " ++ show lid
    leagueUsers <- try $ query (conn env) (getQuery "select * from league_users where league_id = (?)") [lid] :: IO (Either SomeException [LeagueUser])
    case leagueUsers of
        Left e -> do
            logger env ERROR $ "failed to get league users: " ++ show e
            error $ "failed to get league users: " ++ show e
        Right lus -> do
            logger env DEBUG $ "got users for league: " ++ show lid ++ " - length " ++ (show $ length leagueUsers)
            return $ map userId lus

getLeagueByName :: Env -> LeagueName -> IO (Maybe League)
getLeagueByName env ln = do
    logger env DEBUG $ "getting league by name: " ++ show ln
    leagues <- query (conn env) (getQuery "select id, admin_id, name, passcode from leagues l where l.name = (?)") [ln]
    logger env DEBUG $ "got league by name: - length " ++ (show $ map name leagues)
    return $ getSafeHead leagues
    
getLeagueByPasscode :: Env -> LeaguePasscode -> IO (Maybe League)
getLeagueByPasscode env lpc = do
    logger env DEBUG $ "getting league by passcode: " ++ show lpc
    leagues <- query (conn env) (getQuery "select id, admin_id, name, passcode from leagues l where l.passcode = (?)") [lpc]
    logger env DEBUG $ "got league by passcode: - length " ++ (show $ map name leagues)
    return $ getSafeHead leagues

joinLeague :: Env -> UserId -> LeaguePasscode -> IO (Maybe League)
joinLeague env uid ln = do
    logger env DEBUG $ "joining league for user: " ++ show uid
    existingLeague <- getLeagueByPasscode env ln
    case existingLeague of
        Nothing -> return Nothing
        Just l ->
            case League.id l of
                Nothing -> do
                    logger env ERROR $ "expected an id for league: " ++ name l
                    return Nothing
                Just lid -> do
                    res <- try $ returning (conn env) (getQuery "insert into league_users(league_id, user_id) values (?, ?) returning *") [LeagueUser lid uid]  :: IO (Either SomeException [League])
                    case res of
                        Left e -> do
                            logger env ERROR $ "caught e: " ++ show e
                            return Nothing
                        Right r -> do
                            logger env DEBUG $ "created league user for in league: " ++ show r
                            return $ Just $ head r
