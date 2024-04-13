{-# LANGUAGE OverloadedStrings #-}

module Golfer
( Golfer(..)
, Ranking
, GolferName
, GolferId
, filterGolfersById
) where 
import Data.List (partition)
import Text.Mustache (ToMustache (toMustache), object, (~>))

type Ranking = Int
type GolferName = String
type GolferId = Int

data Golfer = Golfer
    { id :: !GolferId
    , ranking :: !Ranking
    , name :: !GolferName
    } deriving (Show)

instance ToMustache Golfer where
    toMustache (Golfer { Golfer.id = i, Golfer.ranking = r, Golfer.name = n }) =
        object 
            [ "id" ~> i
            , "ranking" ~> r
            , "name" ~> n
            ]

filterGolfersById :: [GolferId] -> [Golfer] -> ([Golfer], [Golfer])
filterGolfersById gids = partition (\e -> Golfer.id e `elem` gids)

