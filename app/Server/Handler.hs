{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler where

import Network.Wai
import Servant
-- Adicionamos o 'execute' na importação do PostgreSQL para rodar comandos que não retornam linhas
import Database.PostgreSQL.Simple (Connection, query, query_, execute, Only(..))
import Control.Monad.IO.Class (liftIO)
import Types.Examples (Cripto(..), ResultadoResponse(..))

-- API completa com as 4 operações (CRUD)
type API = "cripto" :> ReqBody '[JSON] Cripto :> Post '[JSON] ResultadoResponse
      :<|> "cripto" :> Get '[JSON] [Cripto]
      -- Rota PUT: recebe o ID na URL e os dados novos no Body
      :<|> "cripto" :> Capture "id" Int :> ReqBody '[JSON] Cripto :> Put '[JSON] ResultadoResponse
      -- Rota DELETE: recebe só o ID na URL
      :<|> "cripto" :> Capture "id" Int :> Delete '[JSON] ResultadoResponse

-- Passamos os 4 handlers para o servidor
server :: Connection -> Server API
server conn = handlerPostCripto conn 
         :<|> handlerGetCriptos conn
         :<|> handlerPutCripto conn
         :<|> handlerDeleteCripto conn

-- 1. CREATE (POST)
handlerPostCripto :: Connection -> Cripto -> Handler ResultadoResponse
handlerPostCripto conn novaCripto = do
    res <- liftIO $ query conn 
           "INSERT INTO criptomoeda (nome, ticker, quantidade, preco_medio) VALUES (?, ?, ?, ?) RETURNING id" 
           (nome novaCripto, ticker novaCripto, quantidade novaCripto, preco_medio novaCripto)
    
    case res of
        [Only newId] -> pure (ResultadoResponse newId)
        _ -> throwError err500 { errBody = "Erro interno ao cadastrar ativo." }

-- 2. READ (GET)
handlerGetCriptos :: Connection -> Handler [Cripto]
handlerGetCriptos conn = do
    lista <- liftIO $ query_ conn "SELECT id, nome, ticker, quantidade, preco_medio FROM criptomoeda ORDER BY id ASC"
    pure lista

-- 3. UPDATE (PUT)
handlerPutCripto :: Connection -> Int -> Cripto -> Handler ResultadoResponse
handlerPutCripto conn idUrl criptoAtualizada = do
    _ <- liftIO $ execute conn 
           "UPDATE criptomoeda SET nome = ?, ticker = ?, quantidade = ?, preco_medio = ? WHERE id = ?" 
           (nome criptoAtualizada, ticker criptoAtualizada, quantidade criptoAtualizada, preco_medio criptoAtualizada, idUrl)
    
    pure (ResultadoResponse idUrl)

-- 4. DELETE (DELETE)
handlerDeleteCripto :: Connection -> Int -> Handler ResultadoResponse
handlerDeleteCripto conn idUrl = do
    _ <- liftIO $ execute conn 
           "DELETE FROM criptomoeda WHERE id = ?" 
           (Only idUrl)
    
    pure (ResultadoResponse idUrl)

app :: Connection -> Application
app conn = serve (Proxy @API) (server conn)