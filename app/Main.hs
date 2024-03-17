{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty (scotty, get, html, ActionM, post, request, body, param, formParam, capture, captureParam, put, ScottyM, redirect, headers, header)
import qualified Data.Text.Lazy as TL
import Templates
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty.Cookie (getCookie, setCookie, makeSimpleCookie)
import Player (Player(..))
import Golfer (getGolfers, GolferId, Golfer(id, name))
import Data.Foldable (find)
import Repo (connect)
import User (User(User, id), createUser, getUserByEmail, getUserById)
import Data.UUID as U
import Session (createSession, Session (sessionId, userId), getSessionById)
import Data.Time (getCurrentTime, utcToLocalTime, utc)
import qualified Data.UUID as UUID
import Data.Text (pack, Text)
import Text.Mustache (Template(Template))
import qualified Data.Text as T


genTempUUID :: IO U.UUID 
genTempUUID = do
    let tmp = case U.fromString "f79d74c3-c24f-488f-b515-0903649a92c5" of
            Nothing -> error "Could not generate uuid"
            Just u -> u
    return tmp

cookieKey :: Text
cookieKey = "majorplayer"

main :: IO ()
main = do
    players <- getGolfers
    conn <- connect "localhost" 5432 "postgres" "password"
    scotty 3000 $ do
        get "/" $ do
            c <- getCookie cookieKey
            c' <- case c of 
                    Nothing -> redirect "/"
                    Just c' -> pure c'
            liftIO $ print $ "found home cookie"
            sessId <- case UUID.fromString $ T.unpack c' of
                    Nothing -> error "Cant get sessionId from cookie"
                    Just s -> return s
            sess <- liftIO $ getSessionById conn sessId
            s' <- case sess of 
                    Nothing -> redirect "/"
                    Just s' -> pure s'
            user <- liftIO $ getUserById conn (userId s')
            case user of 
                Nothing -> redirect "/"
                Just _ -> redirect "/home"
        post "/login" $ do
            email <- formParam "email" :: ActionM String
            liftIO $ print $ "param: " ++ email
            r <- request
            liftIO $ print $ "request: " ++ show r
            existingUser <- liftIO $ getUserByEmail conn email
            liftIO $ print $ "existingUser : " ++ show existingUser
            user <- case existingUser of 
                    Nothing -> liftIO $ createUser conn email
                    Just u -> return u
            let userId = case User.id user of
                    Nothing -> error "Expecting a userID"
                    Just id' -> id'
            time <- liftIO getCurrentTime
            sess <- liftIO $ createSession conn userId (utcToLocalTime utc time)
            let sessId = case sessionId sess of
                    Nothing -> error "Expecting a sessionID"
                    Just sid -> sid
            let c = makeSimpleCookie "majorplayer" (pack $ UUID.toString sessId)
            setCookie c
            redirect "/home"
        get "/home" $ do 
            r <- request
            liftIO $ print r
            host <- header "Host"
            liftIO $ case host of
                Nothing -> print "host not found"
                Just h ->  print $ "host: " ++ show h
            c <- getCookie cookieKey
            c' <- case c of 
                    Nothing -> redirect "/"
                    Just c' -> pure c'
            liftIO $ print $ "found home cookie"
            sessId <- case UUID.fromString $ T.unpack c' of
                    Nothing -> error "Cant get sessionId from cookie"
                    Just s -> return s
            sess <- liftIO $ getSessionById conn sessId
            s' <- case sess of 
                    Nothing -> redirect "/"
                    Just s' -> pure s'
            user <- liftIO $ getUserById conn (userId s')
            u <- case user of 
                    Nothing -> redirect "/"
                    Just u -> pure u
            let player = Player u []
            t <- liftIO $ buildIndex $ UserTemplate (Just player) players
            html $ TL.fromStrict t
        put (capture "/select/:golferId") $ do
            gid <- captureParam "golferId"
            liftIO $ print $ "found golferId: " ++ gid
            c <- getCookie "majorplayer"
            liftIO $ print c
            tmp <- liftIO genTempUUID
            let readGolferId = read gid :: GolferId
                filteredGolfers = filter (\e -> Golfer.id e /= readGolferId) players
                selected = find (\e -> Golfer.id e == readGolferId) players
                sel = case selected of
                    Nothing -> []
                    Just s -> [s]
                user = User (Just tmp) "todo email"
                player = Player user sel
            liftIO $ print ("selected :" ++ (name $ head sel))
            t <- liftIO $ buildHome $ UserTemplate (Just player) filteredGolfers
            html $ TL.fromStrict t
        put (capture "/deselect/:golferId") $ do
            gid <- captureParam "golferId"
            liftIO $ print $ "found golferId: " ++ gid
            c <- getCookie "majorplayer"
            liftIO $ print c
            tmp <- liftIO genTempUUID
            let readGolferId = read gid :: GolferId
                filteredGolfers = filter (\e -> Golfer.id e /= readGolferId) players
                selected = find (\e -> Golfer.id e == readGolferId) players
                sel = case selected of
                    Nothing -> []
                    Just s -> [s]
                user = User (Just tmp) "todo email deselect" 
                player = Player user sel 
            liftIO $ print ("selected :" ++ (name $ head sel))
            t <- liftIO $ buildHome $ UserTemplate (Just player) filteredGolfers
            html $ TL.fromStrict t




