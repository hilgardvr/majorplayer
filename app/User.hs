{-# LANGUAGE OverloadedStrings #-}
module User
( User(..)
, createUser
, UserId
) where
import Text.Mustache (ToMustache (toMustache), object, (~>))
import Database.PostgreSQL.Simple (Connection, ToRow, FromRow, returning)
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
    print $ "created" 
    return $ head user
    


instance ToMustache User where
    toMustache (User i e) = object
        [ "email" ~> e ]
