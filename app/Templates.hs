{-# LANGUAGE OverloadedStrings #-}

module Templates
( index
, buildIndex
, buildSelectTeamPartial
, buildTeamPage
, buildFilteredGolfers
, UserTemplate(..)
) where
import Text.Mustache (automaticCompile, substitute, Template, compileTemplateWithCache, ToMustache (toMustache), object, (~>))
import Data.Text (Text)
import Text.Mustache.Compile (TemplateCache, cacheFromList)
import Player (Player)
import Golfer (Golfer)
import Validation (ValidationError)
import Env (Env (logger), LogLevel (DEBUG))

searchSpace :: [FilePath]
searchSpace = ["./app/templates"]

index :: FilePath
index = "index.mustache"

--home :: FilePath
--home = "home.mustache"

selectTeamPartial :: FilePath
selectTeamPartial = "select-team.mustache"

team :: FilePath
team = "team.mustache"

filteredGolfersPartial :: FilePath
filteredGolfersPartial = "golfers.mustache"

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

buildTemplate :: (ToMustache a) => Env -> FilePath -> a -> IO Text
buildTemplate env f d = do
    logger env DEBUG $ "START :: Compiling " ++ show f
    compiled <- templateOrError f
    logger env DEBUG $ "END :: Compiled " ++ show f
    return $ substitute compiled d

buildIndex :: Env -> UserTemplate -> IO Text
buildIndex env = buildTemplate env index

buildSelectTeamPartial :: Env -> UserTemplate -> IO Text
buildSelectTeamPartial env = buildTemplate env selectTeamPartial

buildTeamPage :: Env -> Player -> IO Text
buildTeamPage env = buildTemplate env team 

buildFilteredGolfers :: Env -> UserTemplate -> IO Text
buildFilteredGolfers env = buildTemplate env filteredGolfersPartial
