{-# LANGUAGE OverloadedStrings #-}

module Player 
(Player(..))
where
import Golfer (Golfer)
import Text.Mustache (ToMustache (toMustache), object, (~>))
import User (User(..))

type Email = String

data Player = Player 
    { user :: User
    , selected :: [Golfer]
    }

instance ToMustache Player where
    toMustache (Player u s) = object
        [ "user" ~> u
        , "selected" ~> s
        ]
