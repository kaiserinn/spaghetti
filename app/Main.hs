{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Web.Scotty ( scotty )
import Routes.Pasta (getPastaBySlug, addPasta, deletePasta, updatePasta)

main :: IO ()
main = do
    loadFile defaultConfig

    scotty 3000 $ do
        getPastaBySlug
        addPasta
        deletePasta
        updatePasta
