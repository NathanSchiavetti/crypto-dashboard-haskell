{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai (Middleware)
import Server.Handler (app)
import Database.PostgreSQL.Simple (connectPostgreSQL)

-- Configuração customizada do CORS para liberar todas as rotas do CRUD
myCors :: Middleware
myCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type"]
        }

main :: IO ()
main = do
    putStrLn "Conectando ao banco de dados..."
    
    conn <- connectPostgreSQL "host=localhost port=5432 user=postgres password=ROOT dbname=meubanco"
    
    putStrLn "Servidor rodando na porta 8080 com CORS liberado."
    
    -- Agora usamos o myCors no lugar do simpleCors
    run 8080 (myCors (app conn))