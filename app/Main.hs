{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Services.Connect (connection)
import Configuration.Dotenv (loadFile, defaultConfig)
import Web.Scotty
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Data.Aeson (ToJSON, FromJSON, object, (.=))
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Database.MySQL.Simple.Result (convert)
import Network.HTTP.Types.Status
import System.Random.MWC (createSystemRandom)
import Data.Maybe (fromMaybe)
import Data.NanoID

data Pasta = Pasta {
    _id :: Maybe Int,
    title :: String,
    content :: String,
    slug :: Maybe String
} deriving (Show, Generic)

instance ToJSON Pasta where
instance FromJSON Pasta where

instance QueryResults Pasta where
    convertResults [fa,fb,fc,fd] [va,vb,vc,vd] = Pasta { _id = a, title = b, content = c, slug = d }
        where !a = convert fa va
              !b = convert fb vb
              !c = convert fc vc
              !d = convert fd vd
    convertResults fs vs  = convertError fs vs 2

main :: IO ()
main = do
    loadFile defaultConfig

    conn <- connection

    scotty 3000 $ do
        get "/pasta/:slug" $ do
        get "/api/pasta/:slug" $ do
            slugParam <- captureParam "slug" :: ActionM String

            result <- liftIO $ try $ do
                query conn "SELECT id, title, content, slug FROM pasta WHERE slug = ?" (Only slugParam) :: IO [Pasta]

            case result of
                Left ex -> do
                    status status500
                    json $ object ["error" .= ("Database error: " ++ show (ex :: SomeException))]
                Right [] -> do
                    status status404
                    json $ object ["error" .= ("No pasta entry found for slug: " ++ slugParam)]
                Right (pasta:_) -> do
                    json pasta

        post "/api/pasta" $ do
            newPasta <- jsonData :: ActionM Pasta

            randomId <- liftIO $ createSystemRandom >>= nanoID
            let newUrl = fromMaybe (show randomId) (slug newPasta)

            result <- liftIO $ try @SomeException $ execute conn
                "INSERT INTO pasta (title, content, slug) VALUES (?, ?, ?)"
                (title newPasta, content newPasta, newUrl)

            case result of
                Left err -> do
                    status status500
                    json (object ["error" .= ("Failed to save pasta" :: String), "details" .= show (err :: SomeException)])
                Right _ -> do
                    status status201
                    json (object ["slug" .= newUrl])
