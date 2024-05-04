{-# LANGUAGE OverloadedStrings #-}

module TeamController
( teamRoutes
) where

import Env (Env (cookieKey, logger), LogLevel (DEBUG, ERROR, WARN))
import Web.Scotty (ScottyM, get, html, redirect, post, capture, put, captureParam, param)
import Web.Scotty.Cookie (getCookie)
import Utils (getUserForSession, getDraftTeamGolfers, mapMaybe)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (UserTemplate(UserTemplate), buildSelectTeamPartial, buildTeamPage, buildFilteredGolfers, buildDisabledPartial)
import qualified Data.Text.Lazy as TL
import Team (getTeamForFixture, Team (golferIds), addTeam)
import Validation (Validatable(validate))
import Golfer (Golfer (name, id), GolferId)
import Player (Player(Player))
import DraftTeam (addDraftPlayer, deleteDraftPlayer, getDraftTeam, DraftTeam (golferId))
import User (id, updateUserDetails)
import Data.Char (toLower)
import Data.List (isInfixOf)
import DataClient (DataClientApi (getPrePostStartDate))
import Data.Time (getCurrentTime, utc, utcToLocalTime)
import Fixture (name, Fixture (id))

teamRoutes :: DataClientApi a => Env -> [Golfer] -> a -> ScottyM ()
teamRoutes env allGolfers client = do

    post "/save-user-details" $ do
        c <- getCookie (cookieKey env)
        user <- liftIO $ getUserForSession env c
        u <- case user of
            Nothing -> redirect "/"
            Just u -> pure u
        case (User.id u) of
            Nothing -> do
                liftIO $ logger env ERROR $ "Expected to find a user with id for session: " ++ show c
                error $ "Expected to find a user with id for session: " ++ show c
            Just uid -> do
                userName <- param "user-name"
                teamName <- param "team-name"
                liftIO $ updateUserDetails env uid userName teamName
                liftIO $ putStrLn userName
                liftIO $ putStrLn teamName
                redirect "/"

    get "/change-team" $ do
        c <- getCookie (cookieKey env)
        user <- liftIO $ getUserForSession env c
        (fixture, _) <- liftIO $ getPrePostStartDate client
        case fixture of
            Nothing ->  do
                t <- liftIO $ buildDisabledPartial env "Selections are disabled" "There are no upcoming fixtures defined"
                html $ TL.fromStrict t
            Just _ -> do
                u <- case user of
                        Nothing -> redirect "/"
                        Just u -> pure u
                (selected, notSelected) <- liftIO $ getDraftTeamGolfers env allGolfers u
                --liftIO $ logger env DEBUG $ "Selected: " ++ (show $ length selected)
                let player = Player u selected
                    validated = validate player
                    ut = UserTemplate (Just player) notSelected validated
                t <- liftIO $ buildSelectTeamPartial env ut
                html $ TL.fromStrict t

    put (capture "/select/:golferId") $ do
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
                t <- liftIO $ buildSelectTeamPartial env $ UserTemplate (Just player) notSelected validation
                html $ TL.fromStrict t

    put (capture "/deselect/:golferId") $ do
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
                liftIO $ logger env DEBUG ("selected :" ++ show (map Golfer.name selected))
                t <- liftIO $ buildSelectTeamPartial env $ UserTemplate (Just player) notSelected validation
                html $ TL.fromStrict t

    post "/save-team" $ do
        c <- getCookie "majorplayer"
        user <- liftIO $ getUserForSession env c
        draftTeam <- liftIO $ getDraftTeam env (mapMaybe  User.id user)
        (notStarted, _) <- liftIO $ getPrePostStartDate client
        let fixture = case notStarted of
                Nothing -> error "no upcoming fixture defined"
                Just f -> f
        case validate draftTeam of
            Nothing -> do
                _ <- liftIO $ addTeam env (map DraftTeam.golferId draftTeam) (Fixture.id fixture) (mapMaybe User.id user) 
                liftIO $ logger env DEBUG "Saved team - redireting"
                redirect "/team"
            Just err -> do
                liftIO $ logger env ERROR $ "Tried to save team but validation failed" ++ err
                redirect "/"

    get "/team" $ do
        c <- getCookie "majorplayer"
        user <- liftIO $ getUserForSession env c
        user' <- case user of
            Nothing -> do
                liftIO $ logger env ERROR "Could not find user to display team"
                redirect "/"
            Just u -> pure u
        (notStarted, started) <- liftIO $ getPrePostStartDate client
        case notStarted of
            Nothing ->  do
                t <- liftIO $ buildDisabledPartial env "Selections are disabled" "There are no upcoming fixtures defined"
                html $ TL.fromStrict t
            Just fixture -> do
                team <- liftIO $ getTeamForFixture env (User.id user') (Fixture.id fixture)
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
        c <- getCookie "majorplayer"
        userMaybe <- liftIO $ getUserForSession env c
        user <- case userMaybe of 
            Nothing -> do
                liftIO $ logger env ERROR "Could not find user to display team"
                redirect "/"
            Just u -> pure u
        search <- param "golfer"
        liftIO $ logger env DEBUG $ "Found golfer searched for: " ++ search
        draftTeam <- liftIO $ getDraftTeam env (User.id user)
        let lower = map toLower
        let golfers =
                if search == ""
                then filter (\e -> not (elem (Golfer.id e) (map (DraftTeam.golferId) draftTeam))) allGolfers
                else filter (\e -> isInfixOf (lower search) (lower (Golfer.name e))
                    && not (elem (Golfer.id e) (map (DraftTeam.golferId) draftTeam))) allGolfers
        liftIO $ logger env DEBUG $ "Found golfers: " ++ (show $ length golfers)
        t <- liftIO $ buildFilteredGolfers env $ UserTemplate Nothing golfers Nothing
        html $ TL.fromStrict t
