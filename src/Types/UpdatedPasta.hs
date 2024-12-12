{-# LANGUAGE DeriveGeneric #-}

module Types.UpdatedPasta (Pasta(..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics ( Generic )

data Pasta = Pasta
  { _id :: Maybe Int,
    title :: String,
    content :: String,
    slug :: Maybe String,
    view_key :: Maybe String,
    edit_key :: Maybe String,
    updated_view_key :: Maybe String,
    updated_edit_key :: Maybe String
  }
  deriving (Show, Generic)

instance ToJSON Pasta
instance FromJSON Pasta
