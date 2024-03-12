{-# LANGUAGE OverloadedStrings #-}

module Golfer
( Golfer(..)
, Ranking
, GolferName
, GolferId
, getPlayers
) where 
import Data.List (elemIndex)


type Ranking = Int
type GolferName = String
type GolferId = Int

data Golfer = Golfer
    { id :: GolferId
    , ranking :: Ranking
    , name :: GolferName
    }

wordsSeperated :: Char -> String -> [String]
wordsSeperated _ [] = []
wordsSeperated c xs = case takeWhile (/=c) xs of
    "" -> []
    s' -> s' : wordsSeperated c (tail $ dropWhile (/=c) xs)

cleanString :: String -> String
cleanString [] = []
cleanString (h:t) = if h == '"' then cleanString t else h : cleanString t

seperateAndClean :: Char -> String -> [String]
seperateAndClean c s = 
    let sep = wordsSeperated c s
    in map cleanString sep


getPlayers :: IO [Golfer]
getPlayers = do
    f <- readFile "downloaded_rankings.csv"
    let ls = lines f
        nameIndex = case elemIndex "\"NAME\"" (wordsSeperated ',' (head ls)) of
            Nothing -> error "Name not found"
            Just i -> i
        rankingIndex = case elemIndex "\"RANKING\"" (wordsSeperated ',' (head ls)) of
            Nothing -> error "Ranking not found"
            Just i -> i
        golferId = case elemIndex "\"Player Id\"" (wordsSeperated ',' (head ls)) of
            Nothing -> error "Ranking not found"
            Just i -> i
        ps = map (\e -> 
            let s = seperateAndClean ',' e in
                Golfer (read (s!!golferId) :: GolferId) (read (s!!rankingIndex) :: Ranking) (s!!nameIndex)
            ) (tail ls)
    return ps
