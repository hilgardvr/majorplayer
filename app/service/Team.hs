{-# LANGUAGE OverloadedStrings #-}

module Team
( Team(..)
, addTeam
, getTeamForFixture
, deleteTeamForFixture
, getTeamsForUsersAndFixture
, CaptainId
) where
import Data.UUID (UUID)
import User (UserId)
import Env (LogLevel(DEBUG, ERROR), Env (logger, conn))
import Database.PostgreSQL.Simple (ToRow, execute, query, FromRow, In (In))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Golfer (GolferId)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Repo (getQuery)
import Database.PostgreSQL.Simple.FromRow (field, FromRow (fromRow))
import Database.PostgreSQL.Simple.Types (PGArray(fromPGArray, PGArray))
import Control.Exception (SomeException, try)
import Fixture (FixtureId)
import Data.Int (Int64)

type CaptainId = GolferId

data Team = Team
    { id :: !(Maybe UUID)
    , userId :: !UserId
    , golferIds :: ![GolferId]
    , tournamentId :: !FixtureId
    , captain :: !GolferId
    } deriving (Show, Eq)


instance ToRow Team where
    toRow (Team i u g t c) = case i of
        Nothing -> [toField u, toField $ PGArray g, toField t, toField c]
        Just i' -> toRow (i', u, PGArray g, t, c)

instance FromRow Team where
    fromRow = Team <$> field <*> field <*> (fromPGArray <$> field) <*> field <*> field

addTeam :: Env -> [GolferId] -> FixtureId -> Maybe UserId -> CaptainId -> IO ()
addTeam env golferIds fixtureId userId captainId = do
    case userId of
        Nothing -> do
            logger env ERROR "no user id supplied, not persisting team"
            return ()
        Just uid -> do
            existing <- getTeamForFixture env userId fixtureId
            _ <- case existing of
                Just e -> deleteTeamForFixture env userId fixtureId
                Nothing -> logger env DEBUG "No existing team found, creating new one"
            logger env DEBUG $ "START :: adding team " ++ show golferIds ++ " for " ++ show userId
            let team = Team Nothing uid golferIds fixtureId captainId
            resp <- execute (conn env) (getQuery "insert into team (user_id, golfer_ids, tournament_id, captain_id) values (?,?,?,?)") team
            logger env DEBUG $ "END :: added team " ++ show golferIds ++ " for " ++ show userId ++ " - resp: " ++ show resp
            return ()

getTeamForFixture :: Env -> Maybe UserId -> FixtureId -> IO (Maybe Team)
getTeamForFixture env userId fixtureId = do
    case userId of 
        Nothing -> do
            logger env ERROR "No userId supplied to get team with"
            error "No userId supplied to get team with"
        Just uid -> do
            logger env DEBUG $ "START :: getting team for " ++ show uid ++ " fixure id: " ++ show fixtureId
            resp <- try $ query (conn env) (getQuery "select * from team where user_id = ? and tournament_id = ?") [toField uid, toField fixtureId] :: IO (Either SomeException [Team])
            case resp of
                Left e -> do
                    logger env ERROR $ "failed to get teams for users: " ++ show e
                    error $ "failed to get teams for users: " ++ show e
                Right r -> do
                    logger env DEBUG $ "END :: got team for " ++ show uid ++ ": " ++ show resp
                    if null r
                    then return Nothing 
                    else return $ Just $ head r

getTeamsForUsersAndFixture :: Env -> [UserId] -> FixtureId -> IO [Team]
getTeamsForUsersAndFixture env userIds fixtureId = do
    logger env DEBUG $ "START :: getting teams for " ++ show userIds
    resp <- try $ query (conn env) (getQuery "select * from team where user_id in ? and tournament_id = ?") $ (In userIds, toField fixtureId) :: IO (Either SomeException [Team])
    case resp of
        Left e -> do
            logger env ERROR $ "failed to get teams for users: " ++ show e
            error $ "failed to get teams for users: " ++ show e
        Right r -> do
            logger env DEBUG $ "got teams for users: " ++ show r
            logger env DEBUG $ "END :: got teams for " ++ show userIds ++ ": " ++ show resp
            return r

deleteTeamForFixture :: Env -> Maybe UserId -> FixtureId -> IO ()
deleteTeamForFixture env userId fixtureId = do
    case userId of
        Nothing -> return ()
        Just _ -> do
            logger env DEBUG $ "START :: deleting team for " ++ show userId
            resp <- try $ execute (conn env) (getQuery "delete from team where user_id = ? and tournament_id = ?") [toField userId, toField fixtureId] :: IO (Either SomeException Int64)
            case resp of
                Left e -> do
                    logger env ERROR $ "failed to delete team " ++ show userId ++ " for tournament: " ++ show fixtureId ++ " - error: " ++ show e
                    error $ "failed to delete team " ++ show userId ++ " for tournament: " ++ show fixtureId ++ " - error: " ++ show e
                Right _ -> do
                    logger env DEBUG $ "END :: deleted team for " ++ show userId ++ " fixture : " ++ show fixtureId
                    return ()

