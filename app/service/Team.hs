{-# LANGUAGE OverloadedStrings #-}

module Team
( Team(..)
, addTeam
, getTeam
, deleteTeam
, getTeamsForUsers
) where
import Data.UUID (UUID)
import User (UserId)
import Env (LogLevel(DEBUG, ERROR), Env (logger, conn))
import Database.PostgreSQL.Simple (ToRow, execute, query, FromRow, Only (Only), In (In))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Golfer (GolferId)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Repo (getQuery)
import Database.PostgreSQL.Simple.FromRow (field, FromRow (fromRow))
import Database.PostgreSQL.Simple.Types (PGArray(fromPGArray, PGArray))
import Validation (Validatable (validate))
import qualified Data.Set as Set
import Text.Mustache (ToMustache (toMustache), object, (~>))
import Control.Exception (SomeException, try)


data Team = Team
    { id :: !(Maybe UUID)
    , userId :: !UserId
    , golferIds :: ![GolferId]
    , tournamentId :: !String
    } deriving (Show, Eq)


instance ToRow Team where
    toRow (Team i u g t) = case i of
        Nothing -> [toField u, toField $ PGArray g, toField t]
        Just i' -> toRow (i', u, PGArray g, t)

instance FromRow Team where
    fromRow = Team <$> field <*> field <*> (fromPGArray <$> field) <*> field

instance ToMustache Team where
    toMustache (Team i uid gids ti) = object
        [ "userId" ~> uid
        , "golferIds" ~> gids
        , "tournamentId" ~> ti
        ]

instance Validatable Team where
    validate (Team i u g t) = 
        let unique = Set.fromList t
        in
            if length unique == 8
            then Nothing
            else Just $ "Expected 8 unique golfers, got " ++ show (length unique)


addTeam :: Env -> [GolferId] -> Maybe UserId -> IO ()
addTeam env golferIds userId = do
    case userId of
        Nothing -> return ()
        Just uid -> do
            existing <- getTeam env userId
            _ <- case existing of
                Just e -> deleteTeam env userId
                Nothing -> logger env DEBUG "No existing team found, creating new one"
            logger env DEBUG $ "START :: adding team " ++ show golferIds ++ " for " ++ show userId
            let team = Team Nothing uid golferIds "masters"
            resp <- execute (conn env) (getQuery "insert into team (user_id, golfer_ids, tournament_id) values (?,?,?)") team
            logger env DEBUG $ "END :: added team " ++ show golferIds ++ " for " ++ show userId ++ " - resp: " ++ show resp
            return ()

getTeam :: Env -> Maybe UserId -> IO (Maybe Team)
getTeam env userId = do
    case userId of 
        Nothing -> do
            logger env ERROR "No userId supplied to get team with"
            error "No userId supplied to get team with"
        Just uid -> do
            logger env DEBUG $ "START :: getting team for " ++ show uid
            resp <- query (conn env) (getQuery "select * from team where user_id = (?)") [uid]
            logger env DEBUG $ "END :: got team for " ++ show uid ++ ": " ++ show resp
            if null resp
            then return Nothing 
            else return $ Just $ head resp

getTeamsForUsers :: Env -> [UserId] -> IO [Team]
getTeamsForUsers env userIds = do
    logger env DEBUG $ "START :: getting teams for " ++ show userIds
    resp <- try $ query (conn env) (getQuery "select * from team where user_id in ?") $ Only $ In userIds :: IO (Either SomeException [Team])
    case resp of
        Left e -> do
            logger env ERROR $ "failed to get teams for users: " ++ show e
            error $ "failed to get teams for users: " ++ show e
        Right r -> do
            logger env DEBUG $ "got teams for users: " ++ show r
            logger env DEBUG $ "END :: got teams for " ++ show userIds ++ ": " ++ show resp
            return r

deleteTeam :: Env -> Maybe UserId -> IO ()
deleteTeam env userId = do
    case userId of
        Nothing -> return ()
        Just _ -> do
            logger env DEBUG $ "START :: deleting team for " ++ show userId
            resp <- execute (conn env) (getQuery "delete from team where user_id = (?)") [toField userId] 
            logger env DEBUG $ "END :: deleted team for " ++ show userId ++ ": response: " ++ show resp
            return ()

