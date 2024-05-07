{-# LANGUAGE OverloadedStrings #-}

module Templates
( index
, buildIndex
, buildSelectTeamPartial
, buildTeamPage
, buildFilteredGolfers
, buildLeaguesPartial
, buildLeaguePartial
, UserTemplate(..)
, buildLoginCodePartial
, buildDetailedTeamPartial
, buildDisabledPartial
) where
import Text.Mustache (automaticCompile, substitute, Template, compileTemplateWithCache, ToMustache (toMustache), object, (~>))
import Data.Text (Text)
import Text.Mustache.Compile (TemplateCache, cacheFromList)
import Player (Player (user))
import Golfer (Golfer)
import Validation (ValidationError)
import Env (Env (logger), LogLevel (DEBUG))
import User (UserId, User (name), Email)
import League (League (adminId), LeagueId)
import Data.List
import DetailedTeam (TeamDetailedDTO)
import Fixture (Fixture)
import Data.UUID (UUID)
import qualified Data.UUID as UUID

searchSpace :: [FilePath]
searchSpace = ["./app/templates"]

index :: FilePath
index = "index.mustache"

selectTeamPartial :: FilePath
selectTeamPartial = "select-team.mustache"

userDetailsPartial :: FilePath
userDetailsPartial = "user-details.mustache"

team :: FilePath
team = "team.mustache"

filteredGolfersPartial :: FilePath
filteredGolfersPartial = "golfers.mustache"

leaguesPartial :: FilePath
leaguesPartial = "leagues.mustache"

leaguePartial :: FilePath
leaguePartial = "league.mustache"

loginCodePartial :: FilePath
loginCodePartial = "login-code.mustache"

detailedTeamPartial :: FilePath
detailedTeamPartial = "detailed-team.mustache"

disabledPartial :: FilePath
disabledPartial = "disabled.mustache"

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

data LeaguesPartial = LeaguesPartial
    { adminLeagues :: ![League]
    , leagues :: ![League]
    }

instance ToMustache LeaguesPartial where
    toMustache (LeaguesPartial al ls) = object
        [ "leagues" ~> ls 
        , "adminLeagues" ~> al
        ]

data LeaguePartial = LeaguePartial
    { fixture :: !Fixture
    , teams :: ![TeamDetailedDTO]
    , leagueId ::UUID
    }

instance ToMustache LeaguePartial where
    toMustache (LeaguePartial fixture teams leagueId) = object
        [ "teams" ~> teams
        , "fixture" ~> fixture
        , "leagueId" ~> UUID.toString leagueId
        ]

data Disabled = Disabled 
    { heading :: !String
    , message :: !String
    }

instance ToMustache Disabled where
    toMustache (Disabled heading message) = object
        [ "heading" ~> heading
        , "message" ~> message
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
buildSelectTeamPartial env userTemplate =
    case player userTemplate of
        Nothing -> error "Expected a user"
        Just p -> 
            case User.name $ Player.user p of
                Nothing -> buildTemplate env userDetailsPartial userTemplate
                Just _ -> buildTemplate env selectTeamPartial userTemplate

buildTeamPage :: Env -> Player -> IO Text
buildTeamPage env = buildTemplate env team 

buildFilteredGolfers :: Env -> UserTemplate -> IO Text
buildFilteredGolfers env = buildTemplate env filteredGolfersPartial

buildLeaguesPartial :: Env -> UserId -> [League] -> IO Text
buildLeaguesPartial env uid ls = 
    let (admin, nonAdmin) = partition (\e -> League.adminId e == uid) ls
    in buildTemplate env leaguesPartial (LeaguesPartial admin nonAdmin)

buildLeaguePartial :: Env -> Fixture -> [TeamDetailedDTO] -> LeagueId -> IO Text
buildLeaguePartial env f ts lid = 
    buildTemplate env leaguePartial $ 
        LeaguePartial 
            { fixture = f
            , teams = ts
            , leagueId = lid
            }

buildDetailedTeamPartial :: Env -> TeamDetailedDTO -> IO Text
buildDetailedTeamPartial env dt = buildTemplate env detailedTeamPartial dt

buildLoginCodePartial :: Env -> Email -> IO Text
buildLoginCodePartial env email = buildTemplate env loginCodePartial email

buildDisabledPartial :: Env -> String -> String -> IO Text
buildDisabledPartial env header msg = buildTemplate env disabledPartial (Disabled header msg)
