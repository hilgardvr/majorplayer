{-# LANGUAGE OverloadedStrings #-}

module DetailedTeam
( TeamDetailedDTO(..)
, TeamGolfer(..)
, buildTeamDetailsDTO
, buildDummyTeamDetails
) where
import User (User(..))
import Golfer (id, name, ranking, Golfer, captain, GolferId, Ranking, GolferName)
import Text.Mustache (ToMustache (toMustache), object, (~>))
import Env (Env)
import Data.List (find, sortBy)
import Team (Team(..))
import Leaderboard (LeaderboardGolfer (playerId))
import qualified Leaderboard as LeaderboardGolfer
import Utils (getTeamGolfers)
import GHC.RTS.Flags (GCFlags(compact))

data TeamGolfer = TeamGolfer
    { id :: !GolferId
    , ranking :: !Ranking
    , name :: !GolferName
    , position :: !Int
    , toPar :: !String
    , status :: !String
    , round :: !Int
    , hole :: !Int
    , captain :: !Bool
    } deriving (Show)

instance ToMustache TeamGolfer where
    toMustache (TeamGolfer id rank name pos toPar status round hole captain) =
        let 
            cap :: String
            cap = if captain then "C" else ""
        in object 
            [ "id" ~> id
            , "ranking" ~> rank
            , "name" ~> name
            , "position" ~> pos
            , "toPar" ~> toPar
            , "status" ~> status
            , "round" ~> round
            , "hole" ~> hole
            , "captain" ~> cap            
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

buildDummyTeamDetails :: User.User -> TeamDetailedDTO
buildDummyTeamDetails user = 
    TeamDetailedDTO 
        { user = user
        , teamGolfers = replicate 8 $ TeamGolfer (-1) (-1) "No Selection" 100 "100" "No Selection" (-1) (-1) False
        , totalRank = 800
        }
        

detailedGolferToLeaderboardGolfer :: Golfer -> LeaderboardGolfer -> TeamGolfer
detailedGolferToLeaderboardGolfer gf lbg =
    let 
        pos = 
            if Golfer.captain gf && (LeaderboardGolfer.position lbg == 1)
            then (-100)
            else LeaderboardGolfer.position lbg
    in
        case LeaderboardGolfer.status lbg of
            "complete" -> TeamGolfer (Golfer.id gf) (Golfer.ranking gf) (Golfer.name gf) pos (show $ LeaderboardGolfer.totalToPar lbg) (LeaderboardGolfer.status lbg) (LeaderboardGolfer.currentRound lbg) (LeaderboardGolfer.holesPlayed lbg) (Golfer.captain gf)
            "active" -> TeamGolfer (Golfer.id gf) (Golfer.ranking gf) (Golfer.name gf) pos (show $ LeaderboardGolfer.totalToPar lbg) (LeaderboardGolfer.status lbg) (LeaderboardGolfer.currentRound lbg) (LeaderboardGolfer.holesPlayed lbg) (Golfer.captain gf)
            _ -> TeamGolfer (Golfer.id gf) (Golfer.ranking gf) (Golfer.name gf) 100 (show $ LeaderboardGolfer.totalToPar lbg) (LeaderboardGolfer.status lbg) (LeaderboardGolfer.currentRound lbg) (LeaderboardGolfer.holesPlayed lbg) (Golfer.captain gf)
                

buildTeamDetailsDTO :: Env -> [Team.Team] -> [User.User] -> [Golfer] -> [LeaderboardGolfer] -> [TeamDetailedDTO]
buildTeamDetailsDTO env teams users gs lg = 
    sortBy (\a b -> compare (totalRank a) (totalRank b)) $ map toTeamDto teams 
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
                --golfers = filter (\g -> Golfer.id g `elem` golferIds team) gs
                teamGolfers = getTeamGolfers gs team
                detailedTeamGolfers :: [TeamGolfer]
                detailedTeamGolfers = map (\g ->
                        let leaderboardGolfer = find (\e -> playerId e == Golfer.id g) lg
                        in case leaderboardGolfer of 
                            Nothing -> TeamGolfer (Golfer.id g) (Golfer.ranking g) (Golfer.name g) 100 "N/A" "N/A" 0 0 (Golfer.captain g)
                            Just lg' -> detailedGolferToLeaderboardGolfer g lg'
                    ) teamGolfers

                sortedTeamGolfers = sortBy (\a b -> compare (position a) (position b)) detailedTeamGolfers
                totalRank = foldr (\e a -> position e + a) 0 detailedTeamGolfers
            in 
                TeamDetailedDTO 
                    { user = user
                    , teamGolfers = sortedTeamGolfers
                    , totalRank = totalRank
                    }
