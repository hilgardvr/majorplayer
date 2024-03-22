{-# LANGUAGE OverloadedStrings #-}

module Templates
( index
, home
, buildIndex
, buildHome
, buildTeamPage
, buildGolfers
, UserTemplate(..)
) where
import Text.Mustache (automaticCompile, substitute, Template, compileTemplateWithCache, ToMustache (toMustache), object, (~>))
import Data.Text (Text)
import Text.Mustache.Compile (TemplateCache, cacheFromList)
import Player (Player)
import Golfer (Golfer)
import Validation (ValidationError)

searchSpace :: [FilePath]
searchSpace = ["./app/templates"]

index :: FilePath
index = "index.mustache"

home :: FilePath
home = "home.mustache"

team :: FilePath
team = "team.mustache"

golfersPartial :: FilePath
golfersPartial = "golfers.mustache"

data UserTemplate = UserTemplate
    { player :: !(Maybe Player)
    , golfers :: ![Golfer]
    , validationError :: !ValidationError
    }

instance ToMustache UserTemplate where
    toMustache (UserTemplate Nothing g v) = object
        [ "golfers" ~> g ]
    toMustache (UserTemplate (Just p) g v) = object
        [ "golfers" ~> g
        , "player" ~> p
        , "validationError" ~> v
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

buildTemplate :: (ToMustache a) => FilePath -> a -> IO Text
buildTemplate f d = do
    --putStrLn $ show $ toMustache d
    compiled <- templateOrError f
    return $ substitute compiled d

buildIndex :: UserTemplate -> IO Text
buildIndex = buildTemplate index

buildHome :: UserTemplate -> IO Text
buildHome = buildTemplate home

buildTeamPage :: Player -> IO Text
buildTeamPage = buildTemplate team 

buildGolfers :: UserTemplate -> IO Text
buildGolfers = buildTemplate golfersPartial
