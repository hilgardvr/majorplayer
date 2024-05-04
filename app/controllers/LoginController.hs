{-# LANGUAGE OverloadedStrings #-}

module LoginController
( loginRoutes
) where
import Golfer (Golfer, filterGolfersById)
import Env (Env (cookieKey, logger, emailPassword, emailUsername, emailHost), LogLevel (DEBUG, ERROR, WARN))
import Web.Scotty (ScottyM, ActionM, get, html, redirect, post, formParam, captureParam)
import Player (Player(Player, user, selected))
import Web.Scotty.Cookie (getCookie, makeSimpleCookie, setCookie, deleteCookie, getCookies)
import Utils (getUserForSession)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (UserTemplate(UserTemplate), buildIndex, buildLoginCodePartial)
import qualified Data.Text.Lazy as TL
import User (getUserByEmail, createUser, User (id, loginCode, email), Email, LoginCode, updateUserLoginCode)
import Session (createSession, Session (id))
import qualified Data.UUID as UUID
import Data.Text (pack, unpack)
import Team (getTeamForFixture, Team (golferIds))
import Validation (Validatable(validate))
import Network.Mail.SMTP (Address(Address), simpleMail, plainTextPart, sendMailWithLoginTLS)
import Text.StringRandom (stringRandomIO)
import DataClient (DataClientApi(getPrePostStartDate))
import Fixture (id)

loginRoutes :: DataClientApi a => Env -> [Golfer] -> a -> ScottyM ()
loginRoutes env allGolfers client = do

    post "/logout" $ do
        c <- getCookie (cookieKey env)
        liftIO $ print $ "logout cookie: " ++ show c
        _ <- deleteCookie (cookieKey env)
        redirect "/"

    post "/login/" $ do
        c <- getCookie (cookieKey env)
        liftIO $ print $ "found cookie: " ++ show c
        user <- liftIO $ getUserForSession env c
        case user of
            Just _ -> redirect "/home"
            Nothing -> do
                email <- formParam "email" :: ActionM String
                liftIO $ print $ "email: " ++ email
                userMaybe <- liftIO $ getUserByEmail env email
                user <- case userMaybe of
                    Nothing -> do
                        liftIO $ createUser env email
                    Just u -> pure u
                userId <- case User.id user of
                    Nothing -> do 
                        liftIO $ logger env ERROR $ "Expected a user id for " ++ email
                        error "Expected a user id"
                    Just i -> pure i
                loginCode <- liftIO generateLoginCode
                liftIO $ sendLoginCodeEmail env email loginCode
                _ <- liftIO $ updateUserLoginCode env userId loginCode
                t <- liftIO $ buildLoginCodePartial env $ UserTemplate (Just $ Player user []) allGolfers Nothing
                html $ TL.fromStrict t

    get "/home" $ do 
        c <- getCookie (cookieKey env)
        liftIO $ print $ "found cookie: " ++ show c
        user <- liftIO $ getUserForSession env c
        liftIO $ print $ "user found: " ++ show user
        u <- case user of
                Nothing -> redirect "/"
                Just u -> pure u
        (notStartedM, _) <- liftIO $ getPrePostStartDate client
        case notStartedM of
            Nothing -> error "no upcoming fixture defined"
            Just notStartedFixture -> do
                team <- liftIO $ getTeamForFixture env (User.id u) (Fixture.id notStartedFixture)
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

    get "/" $ do
        c <- getCookie (cookieKey env)
        cs <- getCookies
        liftIO $ print cs
        liftIO $ print $ "found cookie: " ++ show c
        user <- liftIO $ getUserForSession env c
        case user of
            Nothing -> do
                t <- liftIO $ buildIndex env $ UserTemplate Nothing allGolfers Nothing
                html $ TL.fromStrict t
            Just _ -> redirect "/home"
            
    post "/:email" $ do
        code <- formParam "login-code" :: ActionM String
        liftIO $ print $ "---- code ---- : " ++ code
        email <- captureParam "email"
        existingUser <- liftIO $ getUserByEmail env email
        liftIO $ logger env DEBUG $ "existingUser : " ++ show existingUser
        user <- case existingUser of
                Nothing -> liftIO $ createUser env email
                Just u -> return u
        if User.loginCode user == Just code
        then do
            userId <- case User.id user of
                    Nothing -> do
                        liftIO $ logger env ERROR "Expected a user id"
                        error "Expected a user id"
                    Just uid -> pure uid
            sess <- liftIO $ createSession env userId
            let sessId = case Session.id sess of
                    Nothing -> error "Expecting a session id"
                    Just sid -> sid
            let c = makeSimpleCookie (cookieKey env) (pack $ UUID.toString sessId)
            liftIO $ print $ "cookie: " ++ show c
            _ <- setCookie c
            liftIO $ print $ "cookie set: " ++ show c
            redirect "/"
        else do
            liftIO $ logger env WARN $ "login code does not match for user: " ++ show (User.email user)
            redirect "/"


generateLoginCode :: IO String
generateLoginCode = do
    rand <- stringRandomIO "([A-Z]){8}"
    return $ unpack rand

sendLoginCodeEmail :: Env -> Email -> LoginCode -> IO ()
sendLoginCodeEmail env email loginCode = do
    let fromAddress = Address (Just (pack "MajorPlayer")) (pack "no-reply@majorplayer.com")
        toAddress = Address Nothing (pack email)
        mailBody = plainTextPart (TL.pack loginCode)
        mail = simpleMail fromAddress [toAddress] [] [] (pack "MajorPlayer Login Code") [mailBody]
    sendMailWithLoginTLS (emailHost env) (emailUsername env) (emailPassword env) mail
