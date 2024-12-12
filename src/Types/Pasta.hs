{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Pasta (Pasta(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Database.MySQL.Simple.QueryResults
    ( QueryResults(..), convertError )
import Database.MySQL.Simple.Result (convert)
import GHC.Generics ( Generic )

data Pasta = Pasta {
    _id :: Maybe Int,
    title :: String,
    content :: String,
    slug :: Maybe String,
    view_key :: Maybe String,
    edit_key :: Maybe String
} deriving (Show, Generic)

instance ToJSON Pasta where
instance FromJSON Pasta where

instance QueryResults Pasta where
    convertResults [fa,fb,fc,fd,fe,fg] [va,vb,vc,vd,ve,vg] =
        Pasta { _id = a, title = b, content = c, slug = d, view_key = e, edit_key = g }
            where !a = convert fa va
                  !b = convert fb vb
                  !c = convert fc vc
                  !d = convert fd vd
                  !e = convert fe ve
                  !g = convert fg vg
    convertResults fs vs  = convertError fs vs 2
