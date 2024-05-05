{-# LANGUAGE OverloadedStrings #-}

module Fixture
( FixtureAPIResponse(..)
, Fixture(..)
, FixtureId
, StartedFixture
, NotStartedFixture
) where 
import Data.Time (LocalTime)
import GLDApiMeta (ApiMeta)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:) )
import Text.Mustache (ToMustache (toMustache), object, (~>))

type FixtureId = Int
type TourId = Int
type Season = Int
type NotStartedFixture = Fixture
type StartedFixture = Fixture

data FixtureAPIResponse = FixtureAPIResponse
    { meta :: !ApiMeta
    , results :: ![Fixture]
    }

instance FromJSON FixtureAPIResponse where
    parseJSON = withObject "FixtureAPIResponse" $ \v ->  FixtureAPIResponse
        <$> v .: "meta"
        <*> v .: "results"

data Fixture = Fixture
    { id :: !FixtureId
    , fixureType :: !String
    , status :: !String
    , name :: !String
    , tourId :: !TourId
    , country :: !String
    , course :: !String
    , startDate :: !LocalTime
    , endDate :: !LocalTime
    , season :: !Season
    , timezone :: !String
    , prize :: !String
    , prizeCurrency :: !String
    , updatedAt :: !LocalTime
    } deriving (Show)


instance FromJSON Fixture where
    parseJSON = withObject "Fixture" $ \v ->  Fixture
        <$> v .: "id"
        <*> v .: "type"
        <*> v .: "status"
        <*> v .: "name"
        <*> v .: "tour_id"
        <*> v .: "country"
        <*> v .: "course"
        <*> v .: "start_date"
        <*> v .: "end_date"
        <*> v .: "season"
        <*> v .: "timezone"
        <*> v .: "prize_fund"
        <*> v .: "fund_currency"
        <*> v .: "updated"

instance ToMustache Fixture where
    toMustache (Fixture id fixtureType status name tourId country course startDate endDate season timezone prize prizeCurrency updatedAt) = object
        [ "status" ~> status
        , "name" ~> name
        , "course" ~> course
        , "prize" ~> prize
        , "prizeCurrency" ~> prizeCurrency
        , "start" ~> show startDate
        ]

