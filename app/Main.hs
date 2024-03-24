{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty (scotty, get, html, ActionM, post, request, formParam, capture, captureParam, put, redirect, header, param)
import qualified Data.Text.Lazy as TL
import Templates
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty.Cookie (getCookie, setCookie, makeSimpleCookie, deleteCookie)
import Player (Player(..))
import Golfer (getGolfers, GolferId, Golfer(id, name), filterGolfersById)
import Repo (connect)
import User (User(id), createUser, getUserByEmail, getUserById)
import Session (createSession, Session (userId, id, Session), getSessionById)
import qualified Data.UUID as UUID
import Data.Text (pack, Text)
import qualified Data.Text as T
import Env (getEnv, Env (logger), LogLevel (DEBUG, WARN, ERROR))
import DraftTeam (getDraftTeam, addDraftPlayer, deleteDraftPlayer, DraftTeam (golferId))
import Data.List (partition, isInfixOf)
import Validation (Validatable(validate))
import Team (addTeam, getTeam, Team (golferIds, userId))
import Data.Char (toLower)
import Text.ParserCombinators.ReadPrec (lift)

cookieKey :: Text
cookieKey = "majorplayer"

app :: Env -> IO ()
app env = do
    allGolfers <- getGolfers
    scotty 3000 $ do
        get "/" $ do
            c <- getCookie cookieKey
            user <- liftIO $ getUserForSession env c
            case user of 
                Nothing -> do
                    t <- liftIO $ buildIndex env $ UserTemplate Nothing allGolfers Nothing
                    html $ TL.fromStrict t
                Just _ -> redirect "/home"
        post "/login" $ do
            r <- request
            liftIO $ logger env DEBUG $ show r
            email <- formParam "email" :: ActionM String
            liftIO $ logger env DEBUG $ "param: " ++ email
            r <- request
            liftIO $ print $ "request: " ++ show r
            existingUser <- liftIO $ getUserByEmail env email
            liftIO $ logger env DEBUG $ "existingUser : " ++ show existingUser
            user <- case existingUser of 
                    Nothing -> liftIO $ createUser env email
                    Just u -> return u
            userId <- case User.id user of
                    Nothing -> do
                        liftIO $ logger env ERROR "Expected a user id"
                        error "Expected a user id"
                    Just uid -> pure uid
            sess <- liftIO $ createSession env userId 
            let sessId = case Session.id sess of
                    Nothing -> error "Expecting a session id"
                    Just sid -> sid
            let c = makeSimpleCookie "majorplayer" (pack $ UUID.toString sessId)
            setCookie c
            redirect "/home"
        get "/home" $ do 
            r <- request
            liftIO $ logger env DEBUG $ show r
            host <- header "Host"
            liftIO $ case host of
                Nothing -> logger env DEBUG "host not found"
                Just h ->  logger env DEBUG $ "host: " ++ show h
            c <- getCookie cookieKey
            user <- liftIO $ getUserForSession env c
            u <- case user of 
                    Nothing -> redirect "/"
                    Just u -> pure u
            team <- liftIO $ getTeam env (User.id u)
            let (teamSelected, notSelected) = case team of
                    Nothing -> ([], allGolfers)
                    Just t -> filterGolfersById (Team.golferIds t) allGolfers
            let player = Player u teamSelected
                validated = validate player
            t <- liftIO $ buildIndex env $ UserTemplate (Just player) notSelected validated
            html $ TL.fromStrict t
        get "/change-team" $ do
            r <- request
            liftIO $ logger env DEBUG $ show r
            c <- getCookie "majorplayer"
            user <- liftIO $ getUserForSession env c
            u <- case user of 
                    Nothing -> redirect "/"
                    Just u -> pure u
            (selected, notSelected) <- liftIO $ getDraftTeamGolfers env allGolfers u
            liftIO $ logger env DEBUG $ "Selected: " ++ (show $ length selected)
            let player = Player u selected
                validated = validate player
                ut = UserTemplate (Just player) notSelected validated
            t <- liftIO $ buildSelectTeamPartial env ut
            html $ TL.fromStrict t
        put (capture "/select/:golferId") $ do
            r <- request
            liftIO $ logger env DEBUG $ show r
            gid <- captureParam "golferId"
            liftIO $ logger env DEBUG $ "found golferId: " ++ gid
            c <- getCookie "majorplayer"
            user <- liftIO $ getUserForSession env c
            let readGolferId = read gid :: GolferId
            case user of 
                Nothing -> do
                    liftIO $ logger env WARN $ "Could not find user to select golfer. Session: " ++ show c
                    redirect "/"
                Just u -> do
                    _ <- liftIO $ addDraftPlayer env readGolferId (User.id u) 
                    (selected, notSelected) <- liftIO $ getDraftTeamGolfers env allGolfers u
                    let player = Player u selected
                        validation = validate player
                    liftIO $ logger env DEBUG $ "selected :" ++ show (map name selected)
                    t <- liftIO $ buildSelectTeamPartial env $ UserTemplate (Just player) notSelected validation
                    html $ TL.fromStrict t
        put (capture "/deselect/:golferId") $ do
            r <- request
            liftIO $ logger env DEBUG $ show r
            gid <- captureParam "golferId"
            liftIO $ logger env DEBUG $ "found golferId for deselect: " ++ gid
            c <- getCookie "majorplayer"
            user <- liftIO $ getUserForSession env c
            let readGolferId = read gid :: GolferId
            case user of
                Nothing -> do
                    liftIO $ logger env WARN $ "Could not find user to deselect golfer. Session: " ++ show c
                    redirect "/"
                Just u -> do
                    _ <- liftIO $ deleteDraftPlayer env readGolferId (User.id u) 
                    (selected, notSelected) <- liftIO $ getDraftTeamGolfers env allGolfers u
                    let player = Player u selected
                        validation = validate player
                    liftIO $ logger env DEBUG ("selected :" ++ show (map name selected))
                    t <- liftIO $ buildSelectTeamPartial env $ UserTemplate (Just player) notSelected validation
                    html $ TL.fromStrict t
        post "/save-team" $ do
            r <- request
            liftIO $ logger env DEBUG $ show r
            c <- getCookie "majorplayer"
            user <- liftIO $ getUserForSession env c
            draftTeam <- liftIO $ getDraftTeam env (mapMaybe  User.id user) 
            case validate draftTeam of
                Nothing -> do
                    _ <- liftIO $ addTeam env (map DraftTeam.golferId draftTeam) (mapMaybe User.id user)
                    liftIO $ logger env DEBUG "Saved team - redireting"
                    redirect "/display-team"
                Just err -> do
                    liftIO $ logger env ERROR $ "Tried to save team but validation failed" ++ err
                    redirect "/"
        get "/display-team" $ do
            r <- request
            liftIO $ logger env DEBUG $ show r
            c <- getCookie "majorplayer"
            user <- liftIO $ getUserForSession env c
            user' <- case user of 
                Nothing -> do
                    liftIO $ logger env ERROR "Could not find user to display team"
                    redirect "/"
                Just u -> pure u
            team <- liftIO $ getTeam env (mapMaybe  User.id user) 
            case team of 
                Nothing -> do
                    liftIO $ logger env WARN ("No team found to display for " ++ (show $ User.id user'))
                    t <- liftIO $ buildTeamPage env $ Player user' []
                    html $ TL.fromStrict t
                Just t -> do
                    let playerTeam = filter (\e -> elem (Golfer.id e) (Team.golferIds t)) allGolfers
                    t <- liftIO $ buildTeamPage env $ Player user' playerTeam
                    html $ TL.fromStrict t
        get "/filter-available" $ do
            r <- request
            liftIO $ logger env DEBUG $ show r
            c <- getCookie "majorplayer"
            user <- liftIO $ getUserForSession env c
            search <- param "golfer"
            liftIO $ logger env DEBUG $ "Found golfer searched for: " ++ search
            user' <- case user of 
                Nothing -> do
                    liftIO $ logger env ERROR "Could not find user to display team"
                    redirect "/"
                Just u -> pure u
            draftTeam <- liftIO $ getDraftTeam env (mapMaybe  User.id user) 
            let lower = map toLower
            let golfers = 
                    if search == ""
                    then filter (\e -> not (elem (Golfer.id e) (map (DraftTeam.golferId) draftTeam))) allGolfers
                    else filter (\e -> isInfixOf (lower search) (lower (Golfer.name e))
                        && not (elem (Golfer.id e) (map (DraftTeam.golferId) draftTeam))) allGolfers
            liftIO $ logger env DEBUG $ "Found golfers: " ++ (show $ length golfers)
            t <- liftIO $ buildFilteredGolfers env $ UserTemplate Nothing golfers Nothing
            html $ TL.fromStrict t
        get "/logout" $ do
            _ <- deleteCookie cookieKey
            redirect "/"

mapMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
mapMaybe f a =
    case a of 
        Nothing -> Nothing 
        Just a' -> f a'

getUserForSession :: Env -> Maybe Text -> IO (Maybe User)
getUserForSession env cookie = do
    case cookie of
        Nothing -> return Nothing
        Just c' -> do
            logger env DEBUG "found cookie"
            sessId <- case UUID.fromString $ T.unpack c' of
                    Nothing -> do
                        let err = "Cant get sessionId from cookie: " ++ show c'
                        logger env ERROR  err
                        error  err
                    Just s -> return s
            sess <- getSessionById env sessId
            case sess of 
                Nothing -> return Nothing 
                Just s' -> getUserById env (Session.userId s')

getDraftTeamGolfers :: Env -> [Golfer] -> User -> IO ([Golfer], [Golfer])
getDraftTeamGolfers env g u = do
    draftTeam <- liftIO $ getDraftTeam env (User.id u) 
    let draftTeamIds = map (DraftTeam.golferId) draftTeam
        (selected, notSelected) = partition (\e -> Golfer.id e `elem` draftTeamIds) g
    return (selected, notSelected)


main :: IO ()
main = do
    conn <- connect "localhost" 5432 "postgres" "password"
    let env = getEnv conn
    app env
