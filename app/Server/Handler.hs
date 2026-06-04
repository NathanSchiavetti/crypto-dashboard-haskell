{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler where

import Network.Wai
import Servant
import Database.PostgreSQL.Simple (Connection, query, query_, execute, Only(..))
import Control.Monad.IO.Class (liftIO)
import Types.Examples (OrdemCompra(..), AtivoCarteira(..), ResultadoResponse(..), Usuario(..), Deposito(..))

-- API Expandida
type API = "cripto" :> ReqBody '[JSON] OrdemCompra :> Post '[JSON] ResultadoResponse
      :<|> "cripto" :> Get '[JSON] [AtivoCarteira]
      :<|> "cripto" :> Capture "id" Int :> Delete '[JSON] ResultadoResponse
      :<|> "usuario" :> Get '[JSON] Usuario
      :<|> "deposito" :> ReqBody '[JSON] Deposito :> Post '[JSON] ResultadoResponse

server :: Connection -> Server API
server conn = handlerPostCripto conn 
         :<|> handlerGetCriptos conn
         :<|> handlerDeleteCripto conn
         :<|> handlerGetUsuario conn
         :<|> handlerPostDeposito conn

handlerPostCripto :: Connection -> OrdemCompra -> Handler ResultadoResponse
handlerPostCripto conn ordem = do
    let custoTotal = quantidade ordem * preco_compra ordem
    
    -- Trava de Segurança: Verifica o Saldo BRL antes de comprar
    [Usuario saldoAtual] <- liftIO $ query_ conn "SELECT saldo_brl FROM usuarios WHERE id = 1"
    
    if saldoAtual < custoTotal
        then pure (ResultadoResponse "FALHA_SALDO")
        else do
            _ <- liftIO $ execute conn "INSERT INTO criptomoedas (ticker, nome) VALUES (?, ?) ON CONFLICT (ticker) DO NOTHING" (ticker_moeda ordem, nome_moeda ordem)
            [Only idMoeda] <- liftIO $ query conn "SELECT id FROM criptomoedas WHERE ticker = ?" (Only (ticker_moeda ordem)) :: Handler [Only Int]
            _ <- liftIO $ execute conn "INSERT INTO transacoes (usuario_id, criptomoeda_id, tipo, quantidade, preco_unitario, taxa_aplicada) VALUES (1, ?, 'COMPRA', ?, ?, 0.0)" (idMoeda, quantidade ordem, preco_compra ordem)
            _ <- liftIO $ execute conn "INSERT INTO carteiras (usuario_id, criptomoeda_id, quantidade, preco_medio_compra) VALUES (1, ?, ?, ?) ON CONFLICT (usuario_id, criptomoeda_id) DO UPDATE SET preco_medio_compra = ((carteiras.quantidade * carteiras.preco_medio_compra) + (EXCLUDED.quantidade * EXCLUDED.preco_medio_compra)) / (carteiras.quantidade + EXCLUDED.quantidade), quantidade = carteiras.quantidade + EXCLUDED.quantidade" (idMoeda, quantidade ordem, preco_compra ordem)
            
            -- Desconta o dinheiro da conta do usuário
            _ <- liftIO $ execute conn "UPDATE usuarios SET saldo_brl = saldo_brl - ? WHERE id = 1" (Only custoTotal)
            pure (ResultadoResponse "SUCESSO")

handlerGetCriptos :: Connection -> Handler [AtivoCarteira]
handlerGetCriptos conn = do
    liftIO $ query_ conn "SELECT c.id, m.nome, m.ticker, c.quantidade, c.preco_medio_compra FROM carteiras c JOIN criptomoedas m ON c.criptomoeda_id = m.id WHERE c.usuario_id = 1 ORDER BY c.id ASC"

handlerDeleteCripto :: Connection -> Int -> Handler ResultadoResponse
handlerDeleteCripto conn idCarteira = do
    _ <- liftIO $ execute conn "DELETE FROM carteiras WHERE id = ?" (Only idCarteira)
    pure (ResultadoResponse "Posição liquidada.")

handlerGetUsuario :: Connection -> Handler Usuario
handlerGetUsuario conn = do
    [user] <- liftIO $ query_ conn "SELECT saldo_brl FROM usuarios WHERE id = 1"
    pure user

handlerPostDeposito :: Connection -> Deposito -> Handler ResultadoResponse
handlerPostDeposito conn dep = do
    _ <- liftIO $ execute conn "UPDATE usuarios SET saldo_brl = saldo_brl + ? WHERE id = 1" (Only (valor dep))
    pure (ResultadoResponse "Depósito realizado!")

app :: Connection -> Application
app conn = serve (Proxy @API) (server conn)