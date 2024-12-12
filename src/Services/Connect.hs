module Services.Connect (connection) where

import Database.MySQL.Simple
import System.Environment (getEnv)

connection :: IO Connection
connection = do
    dbHost <- getEnv "DB_HOST"
    dbUser <- getEnv "DB_USER"
    dbPassword <- getEnv "DB_PASSWORD"
    dbDatabase <- getEnv "DB_DATABASE"

    connect defaultConnectInfo {
        connectHost = dbHost,
        connectUser = dbUser,
        connectPassword = dbPassword,
        connectDatabase = dbDatabase
    }
