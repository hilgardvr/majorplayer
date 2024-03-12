{-# LANGUAGE OverloadedStrings #-}

module Templates where
import Text.Mustache (automaticCompile, substitute, Template, compileTemplateWithCache, ToMustache (toMustache), object, (~>))
import Data.Text (Text)
import Text.Mustache.Compile (TemplateCache, cacheFromList)
import Player (Player(..))
import Golfer (Golfer (name, ranking, Golfer))

searchSpace :: [FilePath]
searchSpace = ["./app/templates"]

index :: FilePath
index = "index.mustache"

data User = User
    { player :: Maybe Player
    , golfers :: [Golfer]
    }

instance ToMustache User where
    toMustache (User Nothing g) = object 
        [ "golfers" ~> g ]
    toMustache (User (Just p) g) = object
        [ "golfers" ~> g
        , "player" ~> p
        ]

instance ToMustache Player where
    toMustache (Player e) = object 
        [ "email" ~> e ]

instance ToMustache Golfer where
    toMustache (Golfer { ranking = ranking, name = name }) =
        object 
            [ "ranking" ~> ranking
            , "name" ~> name
            ]

compiledTemplates :: IO TemplateCache
compiledTemplates = do
    compiledIndexTemplate <- automaticCompile searchSpace index
    let tmpls = sequence [compiledIndexTemplate]
    case tmpls of
        Left err -> do
            print $ "Error getting compiled templates: " ++ show err
            error $ "Error getting compiled templates:" ++  show err
        Right ts -> return $ cacheFromList ts

templateOrError :: String -> IO Template
templateOrError tmpl = do
    tc <- compiledTemplates
    tmpl' <- compileTemplateWithCache searchSpace tc tmpl
    case tmpl' of
        Left err -> do
            print $ "Error getting compile with cache:" ++ show err
            error $ "Error getting compile with cache:" ++ show err
        Right t' -> return t'


buildHome :: (ToMustache a) => a -> IO Text
buildHome d = do
    compiled <- templateOrError index
    return $ substitute compiled d
