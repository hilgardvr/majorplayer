{-# LANGUAGE OverloadedStrings #-}

module Player 
(Player(..))
where
import Golfer (Golfer (ranking))
import Text.Mustache (ToMustache (toMustache), object, (~>))
import User (User(..))
import Validation (Validatable (validate))

data Player = Player 
    { user :: User
    , selected :: [Golfer]
    --, team :: [Golfer]
    }

instance ToMustache Player where
    toMustache (Player u s) = object
        [ "user" ~> u
        , "selected" ~> s
        --, "team" ~> t
        ]

instance Validatable Player where
    validate (Player u s) = 
        let selectedRanks = map ranking s
        in if length selectedRanks /= 8
            then Just "You need to select 8 players"
            else Nothing
