{-# LANGUAGE OverloadedStrings #-}

module Golfer
( Golfer(..)
, Ranking
, GolferName
, GolferId
, getGolfers
) where 
import Data.List (elemIndex)
import Text.Mustache (ToMustache (toMustache), object, (~>))


type Ranking = Int
type GolferName = String
type GolferId = Int

data Golfer = Golfer
    { id :: GolferId
    , ranking :: Ranking
    , name :: GolferName
    }

instance ToMustache Golfer where
    toMustache (Golfer { Golfer.id = i, Golfer.ranking = r, Golfer.name = n }) =
        object 
            [ "id" ~> i
            , "ranking" ~> r
            , "name" ~> n
            ]

wordsSeperated :: Char -> String -> [String]
wordsSeperated _ [] = []
wordsSeperated c xs = case takeWhile (/=c) xs of
    [] -> []
    s' -> 
        let tl = dropWhile (/= c) xs
        in 
            if null tl
            then [s']
            else s' : wordsSeperated c (tail tl)

cleanString :: String -> String
cleanString [] = []
cleanString (h:t) = if h == '"' then cleanString t else h : cleanString t

seperateAndClean :: Char -> String -> [String]
seperateAndClean c s = 
    let sep = wordsSeperated c s
    in map cleanString sep


getGolfers :: IO [Golfer]
getGolfers = do
    f <- readFile "downloaded_rankings.csv"
    ls <- return $ take 10 $ lines f
    sep <- return $ wordsSeperated ',' (head ls)  
    --print ("sep" ++ show sep)
    let nameIndex = case elemIndex "\"NAME\"" sep of
            Nothing -> error "Name not found"
            Just i -> i
        rankingIndex = case elemIndex "\"RANKING\"" sep of
            Nothing -> error "Ranking not found"
            Just i -> i
        golferId = 0 --case elemIndex "\"Player Id\"" (wordsSeperated ',' (head ls)) of
            --Nothing -> error "Ranking not found"
            --Just i -> i
        ps = map (\e -> 
            let s = seperateAndClean ',' e in
                Golfer (read (s!!golferId) :: GolferId) (read (s!!rankingIndex) :: Ranking) (s!!nameIndex)
            ) (tail ls)
    return ps
