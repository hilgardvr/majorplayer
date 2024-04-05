{-# LANGUAGE OverloadedStrings #-}
module User
( User(..)
, createUser
, UserId
, getUserById
, getUserByEmail
, getUsersByIds
) where
import Text.Mustache (ToMustache (toMustache), object, (~>))
import Database.PostgreSQL.Simple (Connection, ToRow, FromRow, returning, query, Only (Only), In (In))
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Repo (getQuery)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Env (Env (conn, logger), LogLevel (DEBUG, ERROR))
import Control.Exception (try, SomeException)

type Email = String
type UserId = UUID

data User = User
    { id :: !(Maybe UserId)
    , email :: !Email
    } deriving (Show)

instance ToRow User where
    toRow (User i e) = [toField e]

instance FromRow User where
    fromRow = User <$> field <*> field

instance ToMustache User where
    toMustache (User i e) = object
        [ "email" ~> e ]

instance ToMustache UUID where
    toMustache = toMustache . show 

createUser :: Env -> Email -> IO User
createUser env email = do
    logger env DEBUG $ "creating user for " ++ email
    let u = User Nothing email
    user <- returning (conn env) (getQuery "insert into users(email) values (?) returning *") [u]
    logger env DEBUG $ "created user "  ++ show user
    return $ head user

getUserByEmail :: Env -> Email -> IO (Maybe User)
getUserByEmail env email = do
    logger env DEBUG $ "getting user for email: " ++ email
    user <- query (conn env) (getQuery "select * from users where email = (?)") [email]
    logger env DEBUG $ "user result for email: " ++ show user
    if null user
    then return Nothing
    else return $ Just $ head user

getUserById :: Env -> UserId -> IO (Maybe User)
getUserById env userId = do
    logger env DEBUG $ "getting user by id: " ++ show userId
    user <- query (conn env) (getQuery "select * from users where id = (?)") [userId]
    logger env DEBUG $ "user result for id: " ++ show user
    if null user
    then return Nothing
    else return $ Just $ head user


getUsersByIds :: Env -> [UserId] -> IO [User]
getUsersByIds env userIds = do
    logger env DEBUG $ "getting user by id: " ++ show userIds
    users <- try $ query (conn env) (getQuery "select * from users where id in ?") $ Only $ In userIds :: IO (Either SomeException [User])
    case users of
        Left e -> do
            logger env ERROR $ "error fetching users with ids: " ++ show e
            error $ "error fetching users with ids: " ++ show e
        Right us -> do
            logger env DEBUG $ "found users size " ++ show (map User.id us) ++ " of requested " ++ show userIds
            return us
