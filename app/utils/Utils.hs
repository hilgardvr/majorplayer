{-# LANGUAGE OverloadedStrings #-}

module Utils
( getSafeHead
, mapMaybe
, getUserForSession
, getDraftTeamGolfers
, nowUtc
, add12h
, daySeconds
) where
import User (User (id), getUserById)
import Data.Text (Text, unpack)
import Data.List (partition)
import Env (Env (logger), LogLevel (DEBUG, ERROR))
import qualified Data.UUID as UUID
import Session (getSessionById, Session (userId))
import Golfer (Golfer (id))
import DraftTeam (getDraftTeam, DraftTeam (golferId))
import Data.Time (LocalTime, getCurrentTime, utc, utcToLocalTime, NominalDiffTime, addLocalTime)

getSafeHead :: [a] -> Maybe a
getSafeHead [] = Nothing
getSafeHead (x:_) = Just x

mapMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
mapMaybe f a = f =<< a

getUserForSession :: Env -> Maybe Text -> IO (Maybe User)
getUserForSession env cookie = do
    case cookie of
        Nothing -> return Nothing
        Just c' -> do
            logger env DEBUG $ "found cookie" ++ show c'
            sessId <- case UUID.fromString $ unpack c' of
                    Nothing -> do
                        let err = "Cant get sessionId from cookie: " ++ show c'
                        logger env ERROR  err
                        error err
                    Just s -> return s
            sess <- getSessionById env sessId
            case sess of
                Nothing -> return Nothing
                Just s' -> getUserById env (Session.userId s')


getDraftTeamGolfers :: Env -> [Golfer] -> User -> IO ([Golfer], [Golfer])
getDraftTeamGolfers env g u = do
    draftTeam <- getDraftTeam env (User.id u)
    logger env DEBUG $ show  draftTeam
    let draftTeamIds = map DraftTeam.golferId draftTeam
        (selected, notSelected) = partition (\e -> (Golfer.id e) `elem` draftTeamIds) g
    return (selected, notSelected)

nowUtc :: IO LocalTime
nowUtc = do utcToLocalTime utc <$> getCurrentTime

daySeconds :: NominalDiffTime
daySeconds = 86400

add12h :: LocalTime -> LocalTime
add12h = addLocalTime (daySeconds / 2)
