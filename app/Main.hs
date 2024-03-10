{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty (scotty, get, html)
import qualified Data.Text.Lazy as TL
import Data.List (elemIndex)
import Debug.Trace (trace)
import Templates
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Mustache (ToMustache (toMustache), object, (~>))

type Ranking = Int
type GolferName = String

data Golfer = Golfer
    { ranking :: Ranking
    , name :: GolferName
    }

instance ToMustache Golfer where
    toMustache (Golfer { ranking = ranking, name = name }) =
        object 
            [ "ranking" ~> ranking
            , "name" ~> name
            ]

data GolferData = GolferData 
    { golfers :: [Golfer]
    }

instance ToMustache GolferData where
    toMustache (GolferData { golfers = golfers }) =
        object 
            [ "golfers" ~> golfers ]


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
        ps = map (\e -> 
            let s = seperateAndClean ',' e in
                trace (show "rank:" ++ s!!rankingIndex)
                Golfer (read (s!!rankingIndex) :: Int) (s!!nameIndex) 
            ) (tail ls)
    return ps

main :: IO ()
main = do
    players <- getPlayers
    scotty 3000 $ do
        get "/" $ do
            t <- liftIO $ buildIndex $ GolferData players
            liftIO $ print ("index: " ++ show t)
            html $ TL.fromStrict t --TL.pack (intercalate " - " displayPlayer)

