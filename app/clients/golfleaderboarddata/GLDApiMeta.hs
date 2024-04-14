{-# LANGUAGE OverloadedStrings #-}

module GLDApiMeta
( ApiMeta(..)
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))

data ApiMeta = ApiMeta
    { title :: !String
    , description :: !String
    } deriving (Show)

instance FromJSON ApiMeta where
    parseJSON = withObject "ApiMeta" $ \v -> ApiMeta
        <$> v .: "title"
        <*> v .: "description"
