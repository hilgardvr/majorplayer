{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty (scotty, get, html, ActionM, post, request, formParam, capture, captureParam, put, redirect, header)
import qualified Data.Text.Lazy as TL
import Templates
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty.Cookie (getCookie, setCookie, makeSimpleCookie)
import Player (Player(..))
import Golfer (getGolfers, GolferId, Golfer(id, name))
import Repo (connect)
import User (User(id), createUser, getUserByEmail, getUserById)
import Session (createSession, Session (userId, id), getSessionById)
import qualified Data.UUID as UUID
import Data.Text (pack, Text)
import qualified Data.Text as T
import Env (getEnv, Env (logger), LogLevel (DEBUG, WARN, ERROR))
import DraftTeam (getDraftTeam, addDraftPlayer, deleteDraftPlayer, DraftTeam (golferId))
import Data.List (partition)
import Validation (Validatable(validate))
import Team (addTeam, getTeam, Team (golferIds))
import qualified DraftTeam as Team

cookieKey :: Text
cookieKey = "majorplayer"

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
            logger env DEBUG "found home cookie"
            sessId <- case UUID.fromString $ T.unpack c' of
                    Nothing -> do
                        let err = "Cant get sessionId from cookie: " ++ show c'
                        logger env ERROR  err
                        error  err
                    Just s -> return s
            sess <- getSessionById env sessId
            case sess of 
                Nothing -> return Nothing 
                Just s' -> getUserById env (userId s')

getDraftTeamGolfers :: Env -> [Golfer] -> User -> IO ([Golfer], [Golfer])
getDraftTeamGolfers env g u = do
    draftTeam <- liftIO $ getDraftTeam env (User.id u) 
    let draftTeamIds = map (DraftTeam.golferId) draftTeam
        (selected, notSelected) = partition (\e -> Golfer.id e `elem` draftTeamIds) g
    return (selected, notSelected)

app :: Env -> IO ()
app env = do
    allGolfers <- getGolfers
    scotty 3000 $ do
        get "/" $ do
            c <- getCookie cookieKey
            user <- liftIO $ getUserForSession env c
            case user of 
                Nothing -> do
                    t <- liftIO $ buildIndex $ UserTemplate Nothing allGolfers Nothing
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
            c' <- case c of 
                    Nothing -> redirect "/"
                    Just c' -> pure c'
            liftIO $ logger env DEBUG $ "found home cookie"
            sessId <- case UUID.fromString $ T.unpack c' of
                    Nothing -> error "Cant get sessionId from cookie"
                    Just s -> return s
            sess <- liftIO $ getSessionById env sessId
            s' <- case sess of 
                    Nothing -> redirect "/"
                    Just s' -> pure s'
            user <- liftIO $ getUserById env (userId s')
            u <- case user of 
                    Nothing -> redirect "/"
                    Just u -> pure u
            (selected, notSelected) <- liftIO $ getDraftTeamGolfers env allGolfers u
            let player = Player u selected
                validated = validate player
            t <- liftIO $ buildIndex $ UserTemplate (Just player) notSelected validated
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
                    t <- liftIO $ buildHome $ UserTemplate (Just player) notSelected validation
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
                    t <- liftIO $ buildHome $ UserTemplate (Just player) notSelected validation
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
                    liftIO $ logger env WARN "No team found to display - redirecting"
                    redirect "/"
                Just t -> do
                    let playerTeam = filter (\e -> elem (Golfer.id e) (Team.golferIds t)) allGolfers
                    t <- liftIO $ buildTeamPage $ Player user' playerTeam
                    html $ TL.fromStrict t
            


main :: IO ()
main = do
    conn <- connect "localhost" 5432 "postgres" "password"
    let env = getEnv conn
    app env

