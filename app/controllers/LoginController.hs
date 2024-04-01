{-# LANGUAGE OverloadedStrings #-}

module LoginController
( loginRoutes
) where
import Golfer (Golfer, filterGolfersById)
import Env (Env (cookieKey, logger), LogLevel (DEBUG, ERROR))
import Web.Scotty (ScottyM, ActionM, get, html, redirect, post, formParam)
import Player (Player(Player, user, selected))
import Web.Scotty.Cookie (getCookie, makeSimpleCookie, setCookie, deleteCookie)
import Utils (getUserForSession)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (UserTemplate(UserTemplate), buildIndex)
import qualified Data.Text.Lazy as TL
import User (getUserByEmail, createUser, User (id))
import Session (createSession, Session (id))
import qualified Data.UUID as UUID
import Data.Text (pack)
import Team (getTeam, Team (golferIds))
import Validation (Validatable(validate))

loginRoutes :: Env -> [Golfer] -> ScottyM ()
loginRoutes env allGolfers = do
    get "/" $ do
        c <- getCookie (cookieKey env)
        user <- liftIO $ getUserForSession env c
        case user of
            Nothing -> do
                t <- liftIO $ buildIndex env $ UserTemplate Nothing allGolfers Nothing
                html $ TL.fromStrict t
            Just _ -> redirect "/home"
            
    post "/login" $ do
        email <- formParam "email" :: ActionM String
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
        c <- getCookie (cookieKey env)
        user <- liftIO $ getUserForSession env c
        u <- case user of
                Nothing -> redirect "/"
                Just u -> pure u
        team <- liftIO $ getTeam env (User.id u)
        let (teamSelected, notSelected) = case team of
                Nothing -> ([], allGolfers)
                Just t -> filterGolfersById (Team.golferIds t) allGolfers
        let player = Player 
                { user = u
                , selected = teamSelected
                }
            validated = validate player
        t <- liftIO $ buildIndex env $ UserTemplate (Just player) notSelected validated
        html $ TL.fromStrict t

    get "logout" $ do
        _ <- deleteCookie (cookieKey env)
        redirect "/"
