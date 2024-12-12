{-# LANGUAGE DeriveGeneric #-}

module Types.ViewKey (ViewKey(..)) where

import Data.Aeson (FromJSON)
import GHC.Generics ( Generic )

newtype ViewKey = ViewKey {
    view_key :: Maybe String
} deriving (Generic, Show)

instance FromJSON ViewKey where
