module DraftTeam 
( DraftTeam(..)
, addDraftPlayer
, getDraftTeam
, deleteDraftPlayer
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


data DraftTeam = DraftTeam 
    { id :: !(Maybe UUID)
    , userId :: !UUID
    , golferId :: !Int
    } deriving (Show)

instance ToRow DraftTeam where
    toRow (DraftTeam i u g) = case i of
        Nothing -> [toField u, toField g]
        Just i' -> toRow (i', u, g)

instance FromRow DraftTeam where
    fromRow = DraftTeam <$> field <*> field <*> field

addDraftPlayer :: Env -> Int -> Maybe UserId -> IO ()
addDraftPlayer env golferId userId = do
    case userId of 
        Nothing -> return ()
        Just uid -> do
            logger env DEBUG $ "START :: adding draft player " ++ show golferId ++ " for " ++ show userId
            let draftTeam = DraftTeam Nothing uid golferId
            resp <- execute (conn env) (getQuery "insert into draft_team (user_id, golfer_id) values (?,?)") draftTeam
            logger env DEBUG $ "END :: added draft player " ++ show golferId ++ " for " ++ show userId ++ " - resp: " ++ show resp
            return ()

getDraftTeam :: Env -> Maybe UserId -> IO [DraftTeam]
getDraftTeam env userId = do
    case userId of 
        Nothing -> return []
        Just uid -> do
            logger env DEBUG $ "START :: getting draft team for " ++ show uid
            resp <- query (conn env) (getQuery "select * from draft_team where user_id = (?)") [uid]
            logger env DEBUG $ "END :: got draft team for " ++ show uid ++ ": " ++ show resp
            return resp

deleteDraftPlayer :: Env -> GolferId -> Maybe UserId -> IO ()
deleteDraftPlayer env golferId userId = do
    case userId of
        Nothing -> return ()
        Just _ -> do
            logger env DEBUG $ "START :: deleting draft player for " ++ show userId ++ ": " ++ show golferId
            resp <- execute (conn env) (getQuery "delete from draft_team where user_id = (?) and golfer_id = (?)") [toField userId, toField golferId] 
            logger env DEBUG $ "END :: deleted draft player for " ++ show userId ++ ": " ++ show golferId ++ " - response: " ++ show resp
            return ()
