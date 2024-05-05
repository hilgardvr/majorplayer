{-# LANGUAGE OverloadedStrings #-}

module Player 
(Player(..))
where
import Text.Mustache (ToMustache (toMustache), object, (~>))
import User (User(..))
import Validation (Validatable (validate))
import Golfer (Golfer (ranking))
import Fixture (Fixture)

data Player = Player 
    { user :: !User
    , selected :: ![Golfer]
    , fixture :: !Fixture
    }

instance ToMustache Player where
    toMustache (Player u s f) = object
        [ "user" ~> u
        , "selected" ~> s
        , "fixture" ~> f
        ]

instance Validatable Player where
    validate (Player u s _) = 
        let selectedRanks = map ranking s
            topTen = filter (\e -> e <= 10) selectedRanks
        in if length selectedRanks /= 8
            then Just "You need to select 8 players"
            else if length topTen > 4
                 then Just "You can only select 4 top ten ranked golfers"
                 else Nothing
