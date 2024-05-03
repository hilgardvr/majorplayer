{-# LANGUAGE OverloadedStrings #-}

module DetailedTeam
( TeamDetailedDTO(..)
, TeamGolfer(..)
, buildTeamDetailsDTO
) where
import User (User(..))
import Golfer (id, Golfer (name, ranking), GolferId, Ranking, GolferName)
import Text.Mustache (ToMustache (toMustache), object, (~>))
import Env (Env)
import Data.List (find, sortBy)
import Team (Team(..))
import Leaderboard (LeaderboardGolfer (playerId))
import qualified Leaderboard as LeaderboardGolfer

data TeamGolfer = TeamGolfer
    { id :: !GolferId
    , ranking :: !Ranking
    , name :: !GolferName
    , position :: !Int
    , toPar :: !String
    , status :: !String
    } deriving (Show)

instance ToMustache TeamGolfer where
    toMustache (TeamGolfer id rank name pos toPar status) =
        object 
            [ "id" ~> id
            , "ranking" ~> rank
            , "name" ~> name
            , "position" ~> pos
            , "toPar" ~> toPar
            , "status" ~> status
            ]

data TeamDetailedDTO = TeamDetailedDTO
    { user :: !User
    , teamGolfers :: ![TeamGolfer]
    , totalRank :: !Int
    }

instance ToMustache TeamDetailedDTO where
    toMustache (TeamDetailedDTO user teamGolfer totalRank) = object
        [ "user" ~> user
        , "teamGolfers" ~> teamGolfer
        , "totalRank" ~> totalRank
        ]

buildTeamDetailsDTO :: Env -> [Team.Team] -> [User.User] -> [Golfer] -> [LeaderboardGolfer] -> [TeamDetailedDTO]
buildTeamDetailsDTO env teams users gs lg = map toTeamDto teams 
    where 
        toTeamDto :: Team -> TeamDetailedDTO
        toTeamDto team = 
            let 
                userMaybe = 
                    find (\u -> case User.id u of
                        Nothing -> False
                        Just uid -> Team.userId team == uid
                    ) users
                user = case userMaybe of
                    Nothing -> error "could not find user with id"
                    Just u' -> u'
                golfers = filter (\g -> elem (Golfer.id g) (golferIds team)) gs
                teamGolfers :: [TeamGolfer]
                teamGolfers = map (\g ->
                        let leaderboardGolfer = find (\e -> playerId e == Golfer.id g) lg
                        in case leaderboardGolfer of 
                            Nothing -> 
                                let pos = 100
                                    toPar = "N/A"
                                    status = "N/A"
                                in
                                    TeamGolfer (Golfer.id g) (Golfer.ranking g) (Golfer.name g) 100 toPar status
                                -- error $ "Could not find leaderboard golfer " ++ show (Golfer.id g)
                            Just p -> 
                                let roundStatus = LeaderboardGolfer.status p
                                    status = if roundStatus == "complete"
                                    then "Round: " ++ show (LeaderboardGolfer.currentRound p) ++ " Status: " ++ LeaderboardGolfer.status p
                                    else "Round: " ++ (show $ LeaderboardGolfer.currentRound p) ++ " - Holes played: " ++ (show $ LeaderboardGolfer.holesPlayed p)
                                in
                                    TeamGolfer (Golfer.id g) (Golfer.ranking g) (Golfer.name g) (LeaderboardGolfer.position p) (show $ LeaderboardGolfer.totalToPar p) status
                    ) golfers

                sortedTeamGolfers = sortBy (\a b -> compare (position a) (position b)) teamGolfers
                totalRank = foldr (\e a -> position e + a) 0 teamGolfers
            in 
                TeamDetailedDTO 
                { user = user
                , teamGolfers = sortedTeamGolfers
                , totalRank = totalRank
                }
