{-# LANGUAGE DeriveGeneric #-}

module Types.EditKey (EditKey(..)) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

newtype EditKey = EditKey {
    edit_key :: Maybe String
} deriving (Generic, Show)

instance FromJSON EditKey where
