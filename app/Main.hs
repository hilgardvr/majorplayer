{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty (scotty, get, html, ActionM, post, request, body, param, formParam, capture, captureParam, put, ScottyM)
import qualified Data.Text.Lazy as TL
import Templates
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty.Cookie (getCookie, setCookie, makeSimpleCookie)
import Player (Player(..))
import Golfer (getGolfers, GolferId, Golfer(id, name))
import Data.Foldable (find)
import Repo (connect)
import User (User(User, id), createUser)
import Data.UUID as U
import Session (createSession, Session (sessionId))
import Data.Time (getCurrentTime, utcToLocalTime, utc)
import qualified Data.UUID as UUID
import Data.Text (pack)


genTempUUID :: IO U.UUID 
genTempUUID = do
    let tmp = case U.fromString "f79d74c3-c24f-488f-b515-0903649a92c5" of
            Nothing -> error "Could not generate uuid"
            Just u -> u
    return tmp

main :: IO ()
main = do
    players <- getGolfers
    conn <- connect "localhost" 5432 "postgres" "password"
    scotty 3000 $ do
        get "/" $ do
            c <- getCookie "majorplayer"
            liftIO $ print c
            t <- liftIO $ buildTemplate index $ UserTemplate Nothing players
            html $ TL.fromStrict t
        post "/login" $ do
            email <- formParam "email" :: ActionM String
            liftIO $ print $ "param: " ++ email
            user <- liftIO $ createUser conn email
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
            liftIO $ print sess
            let player = Player user []
            t <- liftIO $ buildTemplate index $ UserTemplate (Just player) players
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
            t <- liftIO $ buildTemplate home $ UserTemplate (Just player) filteredGolfers
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
            t <- liftIO $ buildTemplate home $ UserTemplate (Just player) filteredGolfers
            html $ TL.fromStrict t




