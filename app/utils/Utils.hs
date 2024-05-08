{-# LANGUAGE OverloadedStrings #-}

module Utils
( getSafeHead
, mapMaybe
, getUserForSession
, getDraftTeamGolfers
, nowUtc
, add12h
, daySeconds
, getTeamGolfers
) where
import User (User (id), getUserById)
import Data.Text (Text, unpack)
import Data.List (partition, find)
import Env (Env (logger), LogLevel (DEBUG, ERROR))
import qualified Data.UUID as UUID
import Session (getSessionById, Session (userId))
import Golfer (Golfer (id, captain))
import DraftTeam (getDraftTeam, golferId, captain)
import Data.Time (LocalTime, getCurrentTime, utc, utcToLocalTime, NominalDiffTime, addLocalTime)
import Team (Team (golferIds, captain))

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
    let draftTeamIds = map DraftTeam.golferId draftTeam
        (selected, notSelected) = partition (\e -> (Golfer.id e) `elem` draftTeamIds) g
        captainedSelected = 
            map (\s -> 
                case find (\e -> DraftTeam.golferId e == Golfer.id s) draftTeam of
                    Nothing -> s
                    Just f -> s { Golfer.captain = DraftTeam.captain f }
            ) selected
    return (captainedSelected, notSelected)

getTeamGolfers :: [Golfer] -> Team -> [Golfer]
getTeamGolfers gs t =
    let playerTeam = filter (\e -> Golfer.id e `elem` Team.golferIds t) gs
    in map (\p -> p { Golfer.captain = Team.captain t == Golfer.id p }) playerTeam 
    

nowUtc :: IO LocalTime
nowUtc = do utcToLocalTime utc <$> getCurrentTime

daySeconds :: NominalDiffTime
daySeconds = 86400

add12h :: LocalTime -> LocalTime
add12h = addLocalTime (daySeconds / 2)
