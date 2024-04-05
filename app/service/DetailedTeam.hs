{-# LANGUAGE OverloadedStrings #-}

module DetailedTeam
( TeamDetailedDTO(..)
, buildTeamDetailsDTO
) where
import User (User(..))
import Golfer (Golfer (id))
import Text.Mustache (ToMustache (toMustache), object, (~>))
import Env (Env)
import Data.List (find)
import Team (Team(..))

data TeamDetailedDTO = TeamDetailedDTO
    { user :: !User
    , teamGolfers :: ![Golfer]
    }

instance ToMustache TeamDetailedDTO where
    toMustache (TeamDetailedDTO u tg) = object
        [ "user" ~> u
        , "teamGolfers" ~> tg
        ]

buildTeamDetailsDTO :: Env -> [Team.Team] -> [User.User] -> [Golfer] -> [TeamDetailedDTO]
buildTeamDetailsDTO env ts us gs = map toTeamDto ts
    where 
        toTeamDto :: Team -> TeamDetailedDTO
        toTeamDto t = 
            let 
                userMaybe = 
                    find (\u -> case User.id u of
                        Nothing -> False
                        Just uid -> Team.userId t == uid
                    ) us
                user = case userMaybe of
                    Nothing -> error "could not find user with id" -- ++ show (Team.userId t)
                    Just u' -> u'
                teamGolfers = filter (\g -> elem (Golfer.id g) (golferIds t)) gs
            in 
                TeamDetailedDTO 
                { user = user
                , teamGolfers = teamGolfers
                }
