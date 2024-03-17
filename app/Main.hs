{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty (scotty, get, html, ActionM, post, request, formParam, capture, captureParam, put, redirect, header)
import qualified Data.Text.Lazy as TL
import Templates
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty.Cookie (getCookie, setCookie, makeSimpleCookie)
import Player (Player(..))
import Golfer (getGolfers, GolferId, Golfer(id, name))
import Data.Foldable (find)
import Repo (connect)
import User (User(id), createUser, getUserByEmail, getUserById)
import Session (createSession, Session (sessionId, userId), getSessionById)
import qualified Data.UUID as UUID
import Data.Text (pack, Text)
import qualified Data.Text as T
import Env (getEnv, Env (logger), LogLevel (DEBUG, WARN, ERROR))

cookieKey :: Text
cookieKey = "majorplayer"


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
    
app :: Env -> IO ()
app env = do
    players <- getGolfers
    scotty 3000 $ do
        get "/" $ do
            c <- getCookie cookieKey
            user <- liftIO $ getUserForSession env c
            case user of 
                Nothing -> do
                    t <- liftIO $ buildIndex $ UserTemplate Nothing players
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
            let userId = case User.id user of
                    Nothing -> error "Expecting a userID"
                    Just id' -> id'
            sess <- liftIO $ createSession env userId 
            let sessId = case sessionId sess of
                    Nothing -> error "Expecting a sessionID"
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
            let player = Player u []
            t <- liftIO $ buildIndex $ UserTemplate (Just player) players
            html $ TL.fromStrict t
        put (capture "/select/:golferId") $ do
            r <- request
            liftIO $ logger env DEBUG $ show r
            gid <- captureParam "golferId"
            liftIO $ logger env DEBUG $ "found golferId: " ++ gid
            c <- getCookie "majorplayer"
            user <- liftIO $ getUserForSession env c
            case user of 
                Nothing -> do
                    liftIO $ logger env WARN $ "Could not find user to select golfer. Session: " ++ show c
                    redirect "/"
                Just u -> do
                    let readGolferId = read gid :: GolferId
                        filteredGolfers = filter (\e -> Golfer.id e /= readGolferId) players
                        selected = find (\e -> Golfer.id e == readGolferId) players
                        sel = case selected of
                            Nothing -> []
                            Just s -> [s]
                        player = Player u sel
                    liftIO $ logger env DEBUG $ ("selected :" ++ (name $ head sel))
                    t <- liftIO $ buildHome $ UserTemplate (Just player) filteredGolfers
                    html $ TL.fromStrict t
        put (capture "/deselect/:golferId") $ do
            r <- request
            liftIO $ logger env DEBUG $ show r
            gid <- captureParam "golferId"
            liftIO $ logger env DEBUG $ "found golferId for deselect: " ++ gid
            c <- getCookie "majorplayer"
            user <- liftIO $ getUserForSession env c
            case user of
                Nothing -> do
                    liftIO $ logger env WARN $ "Could not find user to deselect golfer. Session: " ++ show c
                    redirect "/"
                Just user' -> do
                    let readGolferId = read gid :: GolferId
                        filteredGolfers = filter (\e -> Golfer.id e /= readGolferId) players
                        selected = find (\e -> Golfer.id e == readGolferId) players
                        sel = case selected of
                            Nothing -> []
                            Just s -> [s]
                        player = Player user' sel 
                    liftIO $ logger env DEBUG ("selected :" ++ (name $ head sel))
                    t <- liftIO $ buildHome $ UserTemplate (Just player) filteredGolfers
                    html $ TL.fromStrict t


main :: IO ()
main = do
    conn <- connect "localhost" 5432 "postgres" "password"
    let env = getEnv conn
    app env

