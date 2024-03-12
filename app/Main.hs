{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty (scotty, get, html, ActionM, post, request, body, param, formParam)
import qualified Data.Text.Lazy as TL
import Templates
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty.Cookie (getCookie)
import Player (Player(..))
import Golfer (getPlayers)


main :: IO ()
main = do
    players <- getPlayers
    scotty 3000 $ do
        get "/" $ do
            c <- getCookie "majorplayer"
            liftIO $ print c
            t <- liftIO $ buildHome $ User Nothing players
            html $ TL.fromStrict t
        post "/login" $ do
            --req <- request
            --b <- body
            --liftIO $ print req
            --liftIO $ print b
            p <- formParam "email" :: ActionM String
            liftIO $ print $ "param: " ++ p
            t <- liftIO $ buildHome $ User (Just $ Player p) players
            html $ TL.fromStrict t
 
