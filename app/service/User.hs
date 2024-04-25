{-# LANGUAGE OverloadedStrings #-}
module User
( User(..)
, createUser
, UserId
, getUserById
, getUserByEmail
, getUsersByIds
, updateUserDetails
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
type UserName = String
type TeamName = String

data User = User
    { id :: !(Maybe UserId)
    , email :: !Email
    , name :: !(Maybe UserName)
    , teamName :: !(Maybe TeamName)
    } deriving (Show)

instance ToRow User where
    toRow (User _ email name teamName) = [toField email, toField name, toField teamName]

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field

instance ToMustache User where
    toMustache (User i email name teamName) = object
        [ "email" ~> email
        , "name" ~> name
        , "teamName" ~> teamName
        ]

createUser :: Env -> Email -> IO User
createUser env email = do
    logger env DEBUG $ "creating user for " ++ email
    let u = User Nothing email Nothing Nothing
    user <- returning (conn env) (getQuery "insert into users(email, name, team_name) values (?, ?, ?) returning *") [u]
    logger env DEBUG $ "created user "  ++ show user
    return $ head user

updateUserDetails :: Env -> UserId -> UserName -> TeamName -> IO User
updateUserDetails env userId userName teamName = do
    logger env DEBUG $ "updating user for " ++ show userId
    user <- try $ query (conn env) (getQuery "update users set name = ?, team_name = ? where id = ? returning *") (userName, teamName, userId) :: IO (Either SomeException [User])
    case user of
        Left e -> do
            logger env ERROR $ "error updating user details: " ++ show e
            error (show e)
        Right u -> do
            logger env DEBUG $ "updated user "  ++ (show $ head u)
            return $ head u

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
