{-# LANGUAGE OverloadedStrings #-}

module League
( League(..)
, createLeague
, getLeaguesForUser
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

type LeagueId = UUID

data League = League 
    { id :: !(Maybe LeagueId)
    , adminId :: !UserId
    , name :: !String
    , passcode :: !(Maybe String)
    } deriving (Show)

instance ToRow League where
    toRow (League _ aid n pc) = [toField aid, toField n, toField pc]

instance FromRow League where
    fromRow = League <$> field <*> field <*> field <*> field

instance ToMustache League where
    toMustache (League i aid n pc) = object
        [ "id" ~> case i of 
                Nothing -> ""
                Just i' -> show i'
        , "adminId" ~> aid
        , "name" ~> n
        ]

data LeagueUser = LeagueUser
    { leagueId :: !(LeagueId)
    , userId :: !(UserId)
    }

instance ToRow LeagueUser where
    toRow (LeagueUser lid uid) = [toField lid, toField uid]

instance FromRow LeagueUser where
    fromRow = LeagueUser <$> field <*> field

createLeague :: Env -> League -> IO League
createLeague env league = do
    logger env DEBUG $ "creating league " ++ name league
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

--getUserById :: Env -> UserId -> IO (Maybe User)
--getUserById env userId = do
--    logger env DEBUG $ "getting user by id: " ++ show userId
--    user <- query (conn env) (getQuery "select * from users where id = (?)") [userId]
--    logger env DEBUG $ "user result for id: " ++ show user
--    if null user
--    then return Nothing
--    else return $ Just $ head user
