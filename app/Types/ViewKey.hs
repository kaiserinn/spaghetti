{-# LANGUAGE DeriveGeneric #-}

module Types.ViewKey where

import Data.Aeson (FromJSON)
import GHC.Generics

newtype ViewKey = ViewKey {
    view_key :: Maybe String
} deriving (Generic, Show)

instance FromJSON ViewKey where
