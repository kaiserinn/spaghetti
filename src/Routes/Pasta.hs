{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Routes.Pasta (getPastaById, addPasta, deletePasta, updatePasta) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=),)
import Data.Maybe (fromMaybe)
import Data.NanoID ( nanoID )
import Database.MySQL.Simple ( execute, query, Only(Only) )
import Network.HTTP.Types.Status
    ( status500,
      status400,
      status200,
      status201,
      status401,
      status404,
      status409 )
import Services.Connect (connection)
import System.Random.MWC (createSystemRandom)
import UnliftIO.Exception (catch)
import Web.Scotty
    ( ScottyM,
      captureParam,
      delete,
      get,
      json,
      jsonData,
      post,
      put,
      status,
      ActionM )

import qualified Types.Pasta as Pasta
import qualified Types.UpdatedPasta as UP
import qualified Types.ViewKey as VK

returnPasta:: Pasta.Pasta -> Value
returnPasta pasta = object [
    "id" .= Pasta._id pasta,
    "title" .= Pasta.title pasta,
    "content" .= Pasta.content pasta,
    "slug" .= Pasta.slug pasta
    ]

getPastaById :: ScottyM ()
getPastaById = get "/api/pasta/:slug" $ do
    conn <- liftIO connection

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

addPasta :: ScottyM ()
addPasta = post "/api/pasta" $ do
    conn <- liftIO connection

    maybePasta <- catch (jsonData :: ActionM (Maybe Pasta.Pasta)) (\(_ :: SomeException) -> return Nothing)

    case maybePasta of
        Nothing -> do
            status status400
            json (object ["error" .= ("Invalid request body" :: String)])
        Just newPasta -> do
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


deletePasta :: ScottyM ()
deletePasta = delete "/api/pasta/:slug" $ do
    conn <- liftIO connection

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

updatePasta :: ScottyM ()
updatePasta = put "/api/pasta/:id" $ do
    conn <- liftIO connection

    idParam <- captureParam "id" :: ActionM Int
    maybePasta <- catch (jsonData :: ActionM (Maybe UP.Pasta)) (\(_ :: SomeException) -> return Nothing)

    case maybePasta of
        Nothing -> do
            status status400
            json (object ["error" .= ("Invalid request body" :: String)])
        Just updatedPasta -> do
            currEditKey <- liftIO $ try @SomeException $ do
                query conn
                    "SELECT id, title, content, NULL AS slug, NULL AS view_key, edit_key FROM pasta WHERE id = ?"
                    (Only idParam) :: IO [Pasta.Pasta]

            case currEditKey of
                Left ex -> do
                    status status500
                    json $ object ["error" .= ("Database error: " ++ show (ex :: SomeException))]
                Right [] -> do
                    status status404
                    json $ object ["error" .= ("No pasta entry found for id: " ++ show idParam)]
                Right (pasta:_) -> do
                    let currentEditKey = fromMaybe "" (Pasta.edit_key pasta)
                        providedEditKey = fromMaybe "" (UP.edit_key updatedPasta)

                    if currentEditKey == providedEditKey
                        then do
                            let newSlug = UP.slug updatedPasta
                                newTitle = UP.title updatedPasta
                                newContent = UP.content updatedPasta
                                newViewKey = UP.view_key updatedPasta
                                newEditKey = UP.updated_edit_key updatedPasta

                            updateResult <- liftIO $ try @SomeException $ do
                                execute conn
                                    "UPDATE pasta SET title = ?, content = ?, slug = ?, view_key = ?, edit_key = ? WHERE id = ?"
                                    (newTitle, newContent, newSlug, newViewKey, newEditKey, idParam)

                            case updateResult of
                                Left err -> do
                                    status status500
                                    json $ object ["error" .= ("Failed to update pasta" :: String), "details" .= show err]
                                Right _ -> do
                                    status status200
                                    json (object ["message" .= ("Pasta updated successfully" :: String)])
                        else do
                            status status401
                            json $ object ["error" .= ("Invalid edit key" :: String)]
