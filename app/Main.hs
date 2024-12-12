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

newtype ObjectWithViewCode = ObjectWithViewCode {
    _view_key :: Maybe String
} deriving (Generic, Show)

instance FromJSON ObjectWithViewCode where

main :: IO ()
main = do
    loadFile defaultConfig
    conn <- connection

    scotty 3000 $ do
        get "/api/pasta/:slug" $ do
            slugParam <- captureParam "slug" :: ActionM String
            bodyViewKey <- jsonData :: ActionM ObjectWithViewCode

            let viewKey = fromMaybe "" (_view_key bodyViewKey)

            result <- liftIO $ try $ do
                query conn "SELECT id, title, content, slug, view_key, edit_key FROM pasta WHERE slug = ?"
                    (Only slugParam) :: IO [Pasta]

            case result of
                Left ex -> do
                    status status500
                    json $ object ["error" .= ("Database error: " ++ show (ex :: SomeException))]

                Right [] -> do
                    status status404
                    json $ object ["error" .= ("No pasta entry found for slug: " ++ slugParam)]

                Right (pasta:_) -> do
                    let requiredViewCode = view_key pasta
                    case requiredViewCode of
                        Nothing -> json pasta

                        Just "" -> json pasta

                        Just expectedViewCode ->
                            if expectedViewCode == viewKey
                                then json pasta
                                else do
                                    status status401
                                    json $ object ["error" .= ("Invalid or missing view code" :: String)]

        post "/api/pasta" $ do
            newPasta <- jsonData :: ActionM Pasta

            randomId <- liftIO $ createSystemRandom >>= nanoID
            let newSlug = fromMaybe (show randomId) (slug newPasta)
                viewKey = fromMaybe "" (view_key newPasta)
                editKey = fromMaybe "" (edit_key newPasta)

            isExists <- liftIO $ try @SomeException $ do
                query conn "SELECT 1 FROM pasta WHERE slug = ?" (Only newSlug) :: IO [Only Int]

            case isExists of
                Left err -> do
                    status status500
                    json (object ["error" .= ("Failed to check if pasta exists" :: String), "details" .= show (err :: SomeException)])
                Right [] -> do
                    result <- liftIO $ try @SomeException $ execute conn
                        "INSERT INTO pasta (title, content, slug, view_key, edit_key) VALUES (?, ?, ?, ?, ?)"
                        (title newPasta, content newPasta, newSlug, viewKey, editKey)

                    case result of
                        Left err -> do
                            status status500
                            json (object ["error" .= ("Failed to save pasta" :: String), "details" .= show (err :: SomeException)])
                        Right _ -> do
                            status status201
                            json (object ["slug" .= newSlug])
                Right _ -> do
                    status status409
                    json (object ["error" .= ("Pasta with the same slug already exists" :: String)])

        delete "/api/pasta/:slug" $ do
            slugParam <- captureParam "slug" :: ActionM String

            result <- liftIO $ try @SomeException $ do
                execute conn "DELETE FROM pasta WHERE slug = ?" (Only slugParam)

            case result of
                Left err -> do
                    status status500
                    json (object ["error" .= ("Failed to delete pasta" :: String), "details" .= show err])
                Right rowsAffected -> do
                    if rowsAffected == 0
                        then do
                            status status404
                            json (object ["error" .= ("Pasta not found" :: String)])
                        else do
                            status status200
                            json (object ["message" .= ("Pasta deleted successfully" :: String)])
