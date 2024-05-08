{-# LANGUAGE FlexibleInstances #-}

module DraftTeam 
( DraftTeam(..)
, addDraftPlayer
, getDraftTeam
, deleteDraftPlayer
, setDraftCaptain
) where
import Data.UUID (UUID)
import User (UserId)
import Env (LogLevel(DEBUG, ERROR), Env (logger, conn))
import Database.PostgreSQL.Simple (ToRow, execute, query, FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Golfer (GolferId)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Repo (getQuery)
import Database.PostgreSQL.Simple.FromRow (field, FromRow (fromRow))
import Validation (Validatable (validate))
import qualified Data.Set as Set
import Data.Int (Int64)
import Control.Exception (try, SomeException)


data DraftTeam = DraftTeam 
    { id :: !(Maybe UUID)
    , userId :: !UUID
    , golferId :: !GolferId
    , captain :: !(Bool)
    } deriving (Show, Eq, Ord)

instance ToRow DraftTeam where
    toRow (DraftTeam i u g c) = case i of
        Nothing -> [toField u, toField g, toField c]
        Just i' -> toRow (i', u, g, c)

instance FromRow DraftTeam where
    fromRow = DraftTeam <$> field <*> field <*> field <*> field

instance Validatable [DraftTeam] where
    validate draftTeam =
        let uniq = Set.fromList draftTeam
        in 
            if length uniq == 8
            then Nothing
            else Just "Unique draft team does not have a length of 8"
 
addDraftPlayer :: Env -> Int -> Maybe UserId -> Bool -> IO ()
addDraftPlayer env golferId userId isCaptain = do
    case userId of 
        Nothing -> return ()
        Just uid -> do
            logger env DEBUG $ "START :: adding draft player " ++ show golferId ++ " for " ++ show uid
            let draftTeam = DraftTeam Nothing uid golferId isCaptain
            resp <- execute (conn env) (getQuery "insert into draft_team (user_id, golfer_id, is_captain) values (?,?,?)") draftTeam
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

setDraftCaptain :: Env -> GolferId -> UserId -> IO ()
setDraftCaptain env gid uid = do
    logger env DEBUG $ "START :: updating captain for " ++ show uid ++ ": " ++ show gid
    resp <- try $ execute (conn env) (getQuery "update draft_team set is_captain = false where user_id = ? and is_captain = true") [toField uid] :: IO (Either SomeException Int64)
    case resp of 
        Left e -> do
            logger env ERROR $ "failed to unset draft team captain for user : " ++ show uid ++ " - " ++  show e
            error $ show e
        Right r -> do
            setCaptain <- try $ execute (conn env) (getQuery "update draft_team set is_captain = true where user_id = ? and golfer_id = ?") [toField uid, toField gid] :: IO (Either SomeException Int64)
            case setCaptain of
                Left e ->  do
                    logger env ERROR $ "failed to set draft team captain for user : " ++ show uid ++ " - " ++  show e
                    error $ show e
                Right r -> do
                    logger env DEBUG $ "updated draft team captain result for userid: " ++ show uid ++ " - " ++ show r



    

