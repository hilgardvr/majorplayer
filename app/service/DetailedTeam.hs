{-# LANGUAGE OverloadedStrings #-}

module DetailedTeam
( TeamDetailedDTO(..)
, buildTeamDetailsDTO
) where
import User (User(..))
import Golfer (id, Golfer (Golfer, name, ranking), GolferId, Ranking, GolferName)
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
    } deriving (Show)

instance ToMustache TeamGolfer where
    toMustache (TeamGolfer id rank name pos) =
        object 
            [ "id" ~> id
            , "ranking" ~> rank
            , "name" ~> name
            , "position" ~> pos
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
                        let pos = find (\e -> playerId e == Golfer.id g) lg
                        in case pos of 
                            Nothing -> 100 -- error $ "Could not find leaderboard golfer " ++ show (Golfer.id g)
                            Just p -> TeamGolfer (Golfer.id g) (Golfer.ranking g) (Golfer.name g) (LeaderboardGolfer.position p)
                    ) golfers

                sortedTeamGolfers = sortBy (\a b -> compare (position a) (position b)) teamGolfers
                totalRank = foldr (\e a -> position e + a) 0 teamGolfers
            in 
                TeamDetailedDTO 
                { user = user
                , teamGolfers = sortedTeamGolfers
                , totalRank = totalRank
                }
