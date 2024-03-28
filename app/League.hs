{-# LANGUAGE OverloadedStrings #-}

module League
( League(..)
, createLeague
, getLeaguesForUser
, joinLeague
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
import qualified Database.PostgreSQL.Simple.FromField as League

type LeagueId = UUID
type LeagueName = String

data League = League 
    { id :: !(Maybe LeagueId)
    , adminId :: !UserId
    , name :: !LeagueName
    --, passcode :: !(Maybe String)
    } deriving (Show)

instance ToRow League where
    toRow (League _ aid n) = [toField aid, toField n]

instance FromRow League where
    fromRow = League <$> field <*> field <*> field

instance ToMustache League where
    toMustache (League i aid n) = object
        [ "id" ~> maybe "" show i
        , "adminId" ~> aid
        , "name" ~> n
        ]

data LeagueUser = LeagueUser
    { leagueId :: !(LeagueId)
    , userId :: !(UserId)
    } deriving (Show)

instance ToRow LeagueUser where
    toRow (LeagueUser lid uid) = [toField lid, toField uid]

instance FromRow LeagueUser where
    fromRow = LeagueUser <$> field <*> field

createLeague :: Env -> League -> IO League
createLeague env league = do
    logger env DEBUG $ "creating league " ++ name league
    leagueRows <- returning (conn env) (getQuery "insert into leagues(admin_id, name) values (?, ?) returning *") [league]
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
    leagues <- query (conn env) (getQuery "select id, admin_id, name from leagues l join league_users lu on l.id = lu.league_id where lu.user_id = (?)") [uid]
    logger env DEBUG $ "got leagues for user: " ++ show uid ++ " - length " ++ (show $ length leagues)
    return leagues

getLeagueByName :: Env -> LeagueName -> IO (Maybe League)
getLeagueByName env ln = do
    logger env DEBUG $ "getting league by name: " ++ show ln
    leagues <- query (conn env) (getQuery "select id, admin_id, name from leagues l where l.name = (?)") [ln]
    logger env DEBUG $ "got league by name: - length " ++ (show $ length leagues)
    return $ getSafeHead leagues
    

joinLeague :: Env -> UserId -> LeagueName -> IO (Maybe League)
joinLeague env uid ln = do
    logger env DEBUG $ "joining league for user: " ++ show uid
    existingLeague <- getLeagueByName env ln
    case existingLeague of
        Nothing -> return Nothing
        Just l -> do
            case League.id l of
                Nothing -> do
                    logger env ERROR $ "expected an id for league: " ++ name l
                    return Nothing
                Just lid -> do
                    leagueUserRow <- returning (conn env) (getQuery "insert into league_users(league_id, user_id) values (?, ?) returning *") [LeagueUser lid uid] :: IO [LeagueUser]
                    logger env DEBUG $ "created league user for in league: " ++ show leagueUserRow
                    return $ Just l


--getUserById :: Env -> UserId -> IO (Maybe User)
--getUserById env userId = do
--    logger env DEBUG $ "getting user by id: " ++ show userId
--    user <- query (conn env) (getQuery "select * from users where id = (?)") [userId]
--    logger env DEBUG $ "user result for id: " ++ show user
--    if null user
--    then return Nothing
--    else return $ Just $ head user
