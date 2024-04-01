{-# LANGUAGE OverloadedStrings #-}

module TeamController
( teamRoutes
) where

import Env (Env (cookieKey, logger), LogLevel (DEBUG, ERROR, WARN))
import Web.Scotty (ScottyM, get, html, redirect, post, capture, put, captureParam, param)
import Web.Scotty.Cookie (getCookie)
import Utils (getUserForSession, getDraftTeamGolfers, mapMaybe)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (UserTemplate(UserTemplate), buildSelectTeamPartial, buildTeamPage, buildFilteredGolfers)
import qualified Data.Text.Lazy as TL
import Team (getTeam, Team (golferIds), addTeam)
import Validation (Validatable(validate))
import Golfer (Golfer (name, id), GolferId)
import Player (Player(Player))
import DraftTeam (addDraftPlayer, deleteDraftPlayer, getDraftTeam, DraftTeam (golferId))
import User (id)
import Data.Char (toLower)
import Data.List (isInfixOf)

teamRoutes :: Env -> [Golfer] -> ScottyM ()
teamRoutes env allGolfers = do
    get "/change-team" $ do
        c <- getCookie (cookieKey env)
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
        case validate draftTeam of
            Nothing -> do
                _ <- liftIO $ addTeam env (map DraftTeam.golferId draftTeam) (mapMaybe User.id user)
                liftIO $ logger env DEBUG "Saved team - redireting"
                redirect "/display-team"
            Just err -> do
                liftIO $ logger env ERROR $ "Tried to save team but validation failed" ++ err
                redirect "/"

    get "/display-team" $ do
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
