{-# LANGUAGE OverloadedStrings #-}

module LeagueController
( leagueRoutes
) where

import Env (Env (logger), LogLevel (DEBUG, ERROR, INFO))
import Web.Scotty (ScottyM, ActionM, get, html, redirect, post, formParam, captureParam)
import Web.Scotty.Cookie (getCookie)
import Utils (getUserForSession)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Templates (buildLeaguesPartial, buildLeaguePartial, buildDetailedTeamPartial, buildDisabledPartial)
import qualified Data.Text.Lazy as TL
import User (id, getUsersByIds, getUserById)
import League (getLeaguesForUser, createLeague, joinLeague, League (name), getUserIdsForLeague)
import qualified Data.UUID as UUID
import Golfer (Golfer)
import Team (getTeamsForUsersAndFixture, getTeamForFixture)
import DetailedTeam (buildTeamDetailsDTO, buildDummyTeamDetails)
import DataClient (DataClientApi (getFixtureLeaderboard, getPrePostStartDate))
import qualified Fixture

leagueRoutes :: DataClientApi a => Env -> [Golfer] -> a -> ScottyM ()
leagueRoutes env allGolfers client = do
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
        (_, startedM) <- liftIO $ getPrePostStartDate client
        case startedM of
             Nothing -> error "not started fixture found"
             Just started -> do
                teams <- liftIO $ getTeamsForUsersAndFixture env userIds (Fixture.id started) 
                users <- liftIO $ getUsersByIds env userIds
                (_, startedM) <- liftIO $ getPrePostStartDate client
                startedFixture <- case startedM of
                    Nothing -> do
                        liftIO $ logger env ERROR "no started fixture found"
                        error "no started fixture found"
                    Just s -> pure s
                leaderboard <- liftIO $ getFixtureLeaderboard client (Fixture.id startedFixture)
                let teamDetails = buildTeamDetailsDTO env teams users allGolfers leaderboard
                t <- liftIO $ buildLeaguePartial env startedFixture teamDetails lid
                html $ TL.fromStrict t

    get "/league/detailed-team/:userId" $ do
        c <- getCookie "majorplayer"
        userMaybe <- liftIO $ getUserForSession env c
        _ <- case userMaybe of
            Nothing -> do
                liftIO $ logger env ERROR "Could not find user to find league"
                redirect "/"
            Just u -> liftIO $ pure u
        uidStr <- captureParam "userId"
        uidDisplay <- case UUID.fromString uidStr of
                Nothing -> do
                    liftIO $ logger env ERROR $ "Failed to parse uuid from string: " ++ uidStr
                    error $ "Failed to parse uuid from string: " ++ uidStr
                Just i -> do pure i
        displayUserMaybe <- liftIO $ getUserById env uidDisplay
        displayUser <- case displayUserMaybe of
                Nothing -> do
                    liftIO $ logger env ERROR $ "Failed to find user to display: " ++ (show uidDisplay)
                    error $ "Failed to find user to display: " ++ (show uidDisplay)
                Just u' -> do
                    liftIO $ pure u'
        (notStarted, started) <- liftIO $ getPrePostStartDate client
        case started of
            Nothing -> do
                t <- liftIO $ buildDisabledPartial env "No Current Leaderboard" "There are no started fixtures defined"
                html $ TL.fromStrict t
            Just s -> do
                team <- liftIO $ getTeamForFixture env (Just uidDisplay) (Fixture.id s)
                case team of
                    Nothing -> do
                        let detailedTeamDummyTeam = buildDummyTeamDetails displayUser 
                        t <- liftIO $ buildDetailedTeamPartial env detailedTeamDummyTeam
                        html $ TL.fromStrict t
                    Just t -> do
                        leaderboard <- liftIO $ getFixtureLeaderboard client (Fixture.id s)
                        let detailedTeam = buildTeamDetailsDTO env [t] [displayUser] allGolfers leaderboard
                        t <- liftIO $ buildDetailedTeamPartial env (head detailedTeam)
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
