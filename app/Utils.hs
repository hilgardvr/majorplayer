module Utils
( getSafeHead
) where

getSafeHead :: [a] -> Maybe a
getSafeHead [] = Nothing
getSafeHead (x:_) = Just x
