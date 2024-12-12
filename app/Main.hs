{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Web.Scotty
import Routes.Pasta (getPastaById, addPasta, deletePasta)

main :: IO ()
main = do
    loadFile defaultConfig

    scotty 3000 $ do
        getPastaById
        addPasta
        deletePasta
