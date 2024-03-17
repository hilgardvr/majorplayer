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

createUser :: Connection -> Email -> IO User
createUser conn email = do
    print $ "creating user for " ++ email
    let u = User Nothing email
    user <- returning conn (getQuery "insert into users(email) values (?) returning *") [u]
    print $ "created user "  ++ show user
    return $ head user

getUserByEmail :: Connection -> Email -> IO (Maybe User)
getUserByEmail conn email = do
    print $ "getting user for email: " ++ email
    user <- query conn (getQuery "select * from users where email = (?)") [email]
    print $ "user result for email: " ++ show user
    if null user
    then return Nothing
    else return $ Just $ head user

getUserById :: Connection -> UserId -> IO (Maybe User)
getUserById conn userId = do
    print $ "getting user by id: " ++ show userId
    user <- query conn (getQuery "select * from users where id = (?)") [userId]
    print $ "user result for id: " ++ show user
    if null user
    then return Nothing
    else return $ Just $ head user


instance ToMustache User where
    toMustache (User i e) = object
        [ "email" ~> e ]
