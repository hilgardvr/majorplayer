{-# LANGUAGE OverloadedStrings #-}
module User
( User(..)
, createUser
, UserId
, getUserById
, getUserByEmail
) where
import Text.Mustache (ToMustache (toMustache), object, (~>))
import Database.PostgreSQL.Simple (Connection, ToRow, FromRow, returning, query)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Repo (getQuery)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Env (Env (conn, logger), LogLevel (DEBUG))

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


instance ToMustache User where
    toMustache (User i e) = object
        [ "email" ~> e ]
