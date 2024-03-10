{-# LANGUAGE OverloadedStrings #-}

module Golfer
( Golfer(..)
, GolferData(..)
, Ranking
, GolferName
) where 
import Text.Mustache (ToMustache (toMustache), object, (~>))


type Ranking = Int
type GolferName = String

data Golfer = Golfer
    { ranking :: Ranking
    , name :: GolferName
    }

data GolferData = GolferData 
    { golfers :: [Golfer]
    }

instance ToMustache Golfer where
    toMustache (Golfer { ranking = ranking, name = name }) =
        object 
            [ "ranking" ~> ranking
            , "name" ~> name
            ]

instance ToMustache GolferData where
    toMustache (GolferData { golfers = golfers }) =
        object 
            [ "golfers" ~> golfers ]
