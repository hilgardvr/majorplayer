module Session
( Session(..)
, createSession
, getSessionById
, SessionId
) where
import Data.UUID
import Data.Time (LocalTime)
import Database.PostgreSQL.Simple (ToRow, FromRow, returning, query)
import User (UserId)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Repo (getQuery)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Env (Env (conn, logger), LogLevel (DEBUG))

type SessionId = UUID

data Session = Session 
    { id :: !(Maybe UUID)
    , userId :: !UserId
    , expiry :: !(Maybe LocalTime)
    } deriving (Show)

instance ToRow Session where
    toRow (Session i u e) = 
        case i of 
            Nothing -> [toField u, toField e]
            Just id' -> toRow (id', u, e)

instance FromRow Session where
    fromRow = Session <$> field <*> field <*> field

createSession :: Env -> UserId -> IO Session
createSession env userId = do
    let s = Session Nothing userId Nothing 
    logger env DEBUG $ "Creating session for userId: " ++ show userId
    sess <- returning (conn env) (getQuery "insert into sessions(user_id, expiry) values (?,?) returning *") [s]
    logger env DEBUG $ "Created session: " ++ show sess
    return $ head sess


getSessionById :: Env -> SessionId -> IO (Maybe Session)
getSessionById env sessId = do
    logger env DEBUG $ "getting user for session: " ++ show sessId
    sess <- query (conn env) (getQuery "select * from sessions where id = (?)") [sessId] 
    logger env DEBUG $ "sess result for sessId: " ++ show sess
    if Prelude.null sess
    then return Nothing
    else return $ Just $ head sess

