module Team
( Team(..)
, addTeam
, getTeam
, deleteTeam
) where
import Data.UUID (UUID)
import User (UserId)
import Env (LogLevel(DEBUG), Env (logger, conn))
import Database.PostgreSQL.Simple (ToRow, execute, query, FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Golfer (GolferId)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Repo (getQuery)
import Database.PostgreSQL.Simple.FromRow (field, FromRow (fromRow))
import Database.PostgreSQL.Simple.Types (PGArray(fromPGArray, PGArray))


data Team = Team
    { id :: !(Maybe UUID)
    , userId :: !UUID
    , golferIds :: ![Int]
    , tournamentId :: !String
    } deriving (Show, Eq)


instance ToRow Team where
    toRow (Team i u g t) = case i of
        Nothing -> [toField u, toField $ PGArray g, toField t]
        Just i' -> toRow (i', u, PGArray g, t)

instance FromRow Team where
    fromRow = Team <$> field <*> field <*> (fromPGArray <$> field) <*> field


addTeam :: Env -> [GolferId] -> Maybe UserId -> IO ()
addTeam env golferIds userId = do
    case userId of 
        Nothing -> return ()
        Just uid -> do
            logger env DEBUG $ "START :: adding team " ++ show golferIds ++ " for " ++ show userId
            let team = Team Nothing uid golferIds "masters"
            resp <- execute (conn env) (getQuery "insert into team (user_id, golfer_ids, tournament_id) values (?,?,?)") team
            logger env DEBUG $ "END :: added team " ++ show golferIds ++ " for " ++ show userId ++ " - resp: " ++ show resp
            return ()

getTeam :: Env -> Maybe UserId -> IO [Team]
getTeam env userId = do
    case userId of 
        Nothing -> return []
        Just uid -> do
            logger env DEBUG $ "START :: getting draft team for " ++ show uid
            resp <- query (conn env) (getQuery "select * from draft_team where user_id = (?)") [uid]
            logger env DEBUG $ "END :: got draft team for " ++ show uid ++ ": " ++ show resp
            return resp

deleteTeam :: Env -> GolferId -> Maybe UserId -> IO ()
deleteTeam env golferId userId = do
    case userId of
        Nothing -> return ()
        Just _ -> do
            logger env DEBUG $ "START :: deleting draft player for " ++ show userId ++ ": " ++ show golferId
            resp <- execute (conn env) (getQuery "delete from draft_team where user_id = (?) and golfer_id = (?)") [toField userId, toField golferId] 
            logger env DEBUG $ "END :: deleted draft player for " ++ show userId ++ ": " ++ show golferId ++ " - response: " ++ show resp
            return ()
