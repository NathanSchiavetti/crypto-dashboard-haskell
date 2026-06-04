{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors) -- Importação nova
import Server.Handler (app)
import Database.PostgreSQL.Simple (connectPostgreSQL)

main :: IO ()
main = do
    putStrLn "Conectando ao banco de dados..."
    conn <- connectPostgreSQL "host=localhost port=5432 user=postgres password=ROOT dbname=meubanco"
    
    putStrLn "Servidor rodando na porta 8080."
    -- Envolve a aplicação 'app' com o 'simpleCors'
    run 8080 (simpleCors (app conn))