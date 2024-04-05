{-# LANGUAGE OverloadedStrings #-}

module LeagueController
( leagueRoutes
) where

import Env (Env (logger), LogLevel (DEBUG, ERROR, INFO))
import Web.Scotty (ScottyM, ActionM, get, html, redirect, post, formParam, captureParam)
import Web.Scotty.Cookie (getCookie)
import Utils (getUserForSession)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (buildLeaguesPartial, buildLeaguePartial)
import qualified Data.Text.Lazy as TL
import User (id, getUsersByIds)
import League (getLeaguesForUser, createLeague, joinLeague, League (name), getUserIdsForLeague)
import qualified Data.UUID as UUID
import Golfer (Golfer)
import Team (getTeamsForUsers)
import DetailedTeam (buildTeamDetailsDTO)

leagueRoutes :: Env -> [Golfer] -> ScottyM ()
leagueRoutes env allGolfers = do
    get "/leagues" $ do
        c <- getCookie "majorplayer"
        user <- liftIO $ getUserForSession env c
        userId <- case user of
            Nothing -> do
                liftIO $ logger env ERROR "Could not find user to find league"
                redirect "/"
            Just u -> case User.id u of
                Nothing -> do
                    let msg = "Could not find userId"
                    liftIO $ logger env ERROR msg
                    error msg
                Just uid -> return uid
        leagues <- liftIO $ getLeaguesForUser env userId
        t <- liftIO $ buildLeaguesPartial env userId leagues
        html $ TL.fromStrict t

    get "/league/:leagueId" $ do
        c <- getCookie "majorplayer"
        user <- liftIO $ getUserForSession env c
        userId <- case user of
            Nothing -> do
                liftIO $ logger env ERROR "Could not find user to find league"
                redirect "/"
            Just u -> case User.id u of
                Nothing -> do
                    let msg = "Could not find userId"
                    liftIO $ logger env ERROR msg
                    error msg
                Just uid -> return uid
        lidStr <- captureParam "leagueId"
        lid <- case UUID.fromString lidStr of
                Nothing -> do
                    liftIO $ logger env ERROR $ "Failed to parse uuid from string: " ++ lidStr
                    error $ "Failed to parse uuid from string: " ++ lidStr
                Just i -> do return i
        userIds <- liftIO $ getUserIdsForLeague env lid
        teams <- liftIO $ getTeamsForUsers env userIds
        users <- liftIO $ getUsersByIds env userIds
        let teamDetails = buildTeamDetailsDTO env teams users allGolfers
        t <- liftIO $ buildLeaguePartial env teamDetails
        html $ TL.fromStrict t

    post "/create-league" $ do
        c <- getCookie "majorplayer"
        user <- liftIO $ getUserForSession env c
        user' <- case user of
            Nothing -> do
                liftIO $ logger env ERROR "Could not find user to display team"
                redirect "/"
            Just u -> pure u
        let uid = case User.id user' of
                Nothing -> error "Userid not found"
                Just i -> i
        leagueName <- formParam "league-name" :: ActionM String
        liftIO $ logger env DEBUG $ "League name: " ++ leagueName
        _ <- liftIO $ createLeague env uid leagueName
        leagues <- liftIO $ getLeaguesForUser env uid
        t <- liftIO $ buildLeaguesPartial env uid leagues
        html $ TL.fromStrict t

    post "/join-league" $ do
        c <- getCookie "majorplayer"
        user <- liftIO $ getUserForSession env c
        user' <- case user of
            Nothing -> do
                liftIO $ logger env ERROR "Could not find user to display team"
                redirect "/"
            Just u -> pure u
        uid <- case User.id user' of
                Nothing -> do
                    liftIO $ logger env ERROR "Userid not found"
                    error "Userid not found"
                Just i -> return i
        leagueName <- formParam "league-passcode" :: ActionM String
        joinedMaybe <- liftIO $ joinLeague env uid leagueName
        case joinedMaybe of
            Nothing -> liftIO $ logger env INFO "No league found for passcode"
            Just j -> liftIO $ logger env DEBUG $ "User " ++ show uid ++ " joined " ++ League.name j
        leaguesForUser <- liftIO $ getLeaguesForUser env uid
        t <- liftIO $ buildLeaguesPartial env uid leaguesForUser
        html $ TL.fromStrict t
