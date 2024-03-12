module Player 
(Player(..))
where

type Email = String

data Player = Player 
    { email :: Email }
