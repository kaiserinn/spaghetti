{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (fromMaybe)
import Data.NanoID
import Database.MySQL.Simple
import Network.HTTP.Types.Status
import Services.Connect (connection)
import System.Random.MWC (createSystemRandom)
import UnliftIO.Exception (catch)
import Web.Scotty

import qualified Types.Pasta as Pasta
import qualified Types.ViewKey as VK

returnPasta:: Pasta.Pasta -> Value
returnPasta pasta = object [
    "id" .= Pasta._id pasta,
    "title" .= Pasta.title pasta,
    "content" .= Pasta.content pasta,
    "slug" .= Pasta.slug pasta
    ]

main :: IO ()
main = do
    loadFile defaultConfig
    conn <- connection

    scotty 3000 $ do
        get "/api/pasta/:slug" $ do
            slugParam <- captureParam "slug" :: ActionM String
            bodyViewKey <- catch (jsonData :: ActionM VK.ViewKey) (\(_ :: SomeException) -> return VK.ViewKey { VK.view_key = Nothing })

            let viewKey = fromMaybe "" (VK.view_key bodyViewKey)

            result <- liftIO $ try $ do
                query conn "SELECT id, title, content, slug, view_key, NULL AS edit_key FROM pasta WHERE slug = ?"
                    (Only slugParam) :: IO [Pasta.Pasta]

            case result of
                Left ex -> do
                    status status500
                    json $ object ["error" .= ("Database error: " ++ show (ex :: SomeException))]

                Right [] -> do
                    status status404
                    json $ object ["error" .= ("No pasta entry found for slug: " ++ slugParam)]

                Right (pasta:_) -> do
                    let requiredViewCode = Pasta.view_key pasta
                    case requiredViewCode of
                        Nothing -> json $ returnPasta pasta

                        Just "" -> json $ returnPasta pasta

                        Just expectedViewCode ->
                            if expectedViewCode == viewKey
                                then  json $ returnPasta pasta
                                else do
                                    status status401
                                    json $ object ["error" .= ("Invalid or missing view code" :: String)]

        post "/api/pasta" $ do
            newPasta <- jsonData :: ActionM Pasta.Pasta

            randomId <- liftIO $ createSystemRandom >>= nanoID
            let slugValue = Pasta.slug newPasta
                newSlug = case slugValue of
                    Just s | not (null s) -> s
                    _                     -> show randomId
                viewKey = fromMaybe "" (Pasta.view_key newPasta)
                editKey = fromMaybe "" (Pasta.edit_key newPasta)

            isExists <- liftIO $ try @SomeException $ do
                query conn "SELECT 1 FROM pasta WHERE slug = ?" (Only newSlug) :: IO [Only Int]

            case isExists of
                Left err -> do
                    status status500
                    json (object ["error" .= ("Failed to check if pasta exists" :: String), "details" .= show (err :: SomeException)])
                Right [] -> do
                    result <- liftIO $ try @SomeException $ execute conn
                        "INSERT INTO pasta (title, content, slug, view_key, edit_key) VALUES (?, ?, ?, ?, ?)"
                        (Pasta.title newPasta, Pasta.content newPasta, newSlug, viewKey, editKey)

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
