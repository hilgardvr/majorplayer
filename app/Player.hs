module Player where


type Ranking = Int
type PlayerName = String

data Player = Player
    { ranking :: Ranking
    , name :: PlayerName
    }

instance ToMustache Player where
    toMustache (Player { ranking = ranking, name = name }) =
        object 
            [ "ranking" ~> ranking
            , "name" ~> name
            ]

