module Session
( Session(..)
, createSession
, getSessionById
, SessionId
) where
import Data.UUID
import Data.Time (LocalTime)
import Database.PostgreSQL.Simple (Connection, ToRow, FromRow, returning, query)
import User (UserId)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Repo (getQuery)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField(toField))

type SessionId = UUID

data Session = Session 
    { id :: !(Maybe UUID)
    , userId :: !UserId
    , sessionId :: !(Maybe SessionId)
    , expiry :: !(Maybe LocalTime)
    } deriving (Show)

instance ToRow Session where
    toRow (Session i u s e) = 
        case i of 
            Nothing -> [toField u, toField e]
            Just id' -> toRow (id', u,  s, e)

instance FromRow Session where
    fromRow = Session <$> field <*> field <*> field <*> field

createSession :: Connection -> UserId -> LocalTime -> IO Session
createSession conn userId expiry = do
    let s = Session Nothing userId Nothing (Just expiry)
    print $ "Createing session for userId: " ++ show userId
    sess <- returning conn (getQuery "insert into sessions(user_id, expiry) values (?,?) returning *") [s]
    print $ "Created session: " ++ show sess
    return $ head sess


getSessionById :: Connection -> SessionId -> IO (Maybe Session)
getSessionById conn sessId = do
    print $ "getting user for session: " ++ show sessId
    sess <- query conn (getQuery "select * from sessions where session_id = (?)") [sessId] 
    print $ "sess result for sessId: " ++ show sess
    if Prelude.null sess
    then return Nothing
    else return $ Just $ head sess

