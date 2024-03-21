module Validation
( ValidationError
, Validatable(..)
, validate
) where

type ValidationError = Maybe String

class Validatable a where
    validate :: a -> ValidationError
