{-# LANGUAGE OverloadedStrings #-}

module LoginController
( loginRoutes
) where
import Golfer (Golfer, filterGolfersById)
import Env (Env (cookieKey, logger, emailPassword, emailUsername, emailHost), LogLevel (DEBUG, ERROR, WARN))
import Web.Scotty (ScottyM, ActionM, get, html, redirect, post, formParam, captureParam)
import Player (Player(Player, user, selected, fixture))
import Web.Scotty.Cookie (getCookie, makeSimpleCookie, setCookie, deleteCookie, getCookies, SetCookie (setCookieExpires, setCookieName, setCookieValue), defaultSetCookie)
import Utils (getUserForSession, nowUtc, daySeconds, getTeamGolfers)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (UserTemplate(UserTemplate), buildIndex, buildLoginCodePartial)
import User (getUserByEmail, createUser, User (id, loginCode, email), Email, LoginCode, updateUserLoginCode)
import Session (createSession, Session (id))
import qualified Data.UUID as UUID
import qualified Data.Text as T --(pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.UTF8 as BSU
import Team (getTeamForFixture, Team (golferIds))
import Validation (Validatable(validate))
import Network.Mail.SMTP (Address(Address), simpleMail, plainTextPart, sendMailWithLoginTLS)
import Text.StringRandom (stringRandomIO)
import DataClient (DataClientApi(getPrePostStartDate))
import Fixture (id, Fixture (startDate))
import Data.Time (addUTCTime, getCurrentTime, addLocalTime)

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
                t <- liftIO $ buildLoginCodePartial env email 
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
                        Just t -> 
                            let (sel, notSel) = filterGolfersById (Team.golferIds t) allGolfers
                                captainedTeam = getTeamGolfers sel t
                            in (captainedTeam, notSel)
                    updatedTimeNotStartedFixture = notStartedFixture { startDate = addLocalTime (daySeconds / 2) (startDate notStartedFixture) }
                let player = Player 
                        { user = u
                        , selected = teamSelected
                        , fixture = updatedTimeNotStartedFixture
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
            now <- liftIO getCurrentTime
            _ <- setCookie $ defaultSetCookie 
                { setCookieName = TE.encodeUtf8 $ cookieKey env
                , setCookieValue = BSU.fromString $ UUID.toString sessId
                , setCookieExpires = Just (addUTCTime (daySeconds * 365) now)
                }
            redirect "/"
        else do
            liftIO $ logger env WARN $ "login code does not match for user: " ++ show (User.email user)
            redirect "/"


generateLoginCode :: IO String
generateLoginCode = do
    rand <- stringRandomIO "([A-Z]){8}"
    return $ T.unpack rand

sendLoginCodeEmail :: Env -> Email -> LoginCode -> IO ()
sendLoginCodeEmail env email loginCode = do
    let fromAddress = Address (Just (T.pack "MajorPlayer")) (T.pack "no-reply@majorplayer.com")
        toAddress = Address Nothing (T.pack email)
        mailBody = plainTextPart (TL.pack loginCode)
        mail = simpleMail fromAddress [toAddress] [] [] (T.pack "MajorPlayer Login Code") [mailBody]
    sendMailWithLoginTLS (emailHost env) (emailUsername env) (emailPassword env) mail
