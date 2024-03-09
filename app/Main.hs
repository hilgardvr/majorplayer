{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty (scotty, get, html)
import qualified Data.Text.Lazy as TL
import Data.List (elemIndex, intercalate)
import Debug.Trace (trace)


class DisplayData a where
    display :: a -> String

type Ranking = Int
type PlayerName = String

data Player = Player
    { ranking :: Ranking
    , name :: PlayerName
    }

instance Show Player where
    show p = show (ranking p) ++ " - " ++ name p

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
    let
        sep = wordsSeperated c s
    in
        map cleanString sep

getPlayers :: IO [Player]
getPlayers = do
    f <- readFile "downloaded_rankings.csv"
    let ls = lines f
        nameIndex = case elemIndex "\"NAME\"" (wordsSeperated ',' (head ls)) of
            Nothing -> error "Name not found"
            Just i -> i
        rankingIndex = case elemIndex "\"RANKING\"" (wordsSeperated ',' (head ls)) of
            Nothing -> error "Ranking not found"
            Just i -> i
        ps = map (\e -> 
            let s = seperateAndClean ',' e in
                trace (show "rank:" ++ s!!rankingIndex)
                Player (read (s!!rankingIndex) :: Int) (s!!nameIndex) 
            ) (tail ls)
    return ps

main :: IO ()
main = do
    players <- getPlayers
    print players
    let displayPlayer = map show players
    scotty 3000 $ do
        get "/" $ do
            html $ TL.pack (intercalate "\n" displayPlayer)


api :: String
api = "Hello, Haskell!"
