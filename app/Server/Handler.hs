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
import Types.Examples (OrdemCompra(..), AtivoCarteira(..), ResultadoResponse(..))

-- Nossa API agora tem 3 rotas (O PUT saiu, pois em corretoras você faz novas compras, não "edita" o passado)
type API = "cripto" :> ReqBody '[JSON] OrdemCompra :> Post '[JSON] ResultadoResponse
      :<|> "cripto" :> Get '[JSON] [AtivoCarteira]
      :<|> "cripto" :> Capture "id" Int :> Delete '[JSON] ResultadoResponse

server :: Connection -> Server API
server conn = handlerPostCripto conn 
         :<|> handlerGetCriptos conn
         :<|> handlerDeleteCripto conn

-- 1. MOTOR DE COMPRA (POST)
handlerPostCripto :: Connection -> OrdemCompra -> Handler ResultadoResponse
handlerPostCripto conn ordem = do
    -- Passo A: Cadastra a moeda se não existir (ignora se já existir)
    _ <- liftIO $ execute conn 
           "INSERT INTO criptomoedas (ticker, nome) VALUES (?, ?) ON CONFLICT (ticker) DO NOTHING" 
           (ticker_moeda ordem, nome_moeda ordem)
    
    -- Passo B: Pega o ID da moeda que acabou de ser criada ou que já existia
    [Only idMoeda] <- liftIO $ query conn "SELECT id FROM criptomoedas WHERE ticker = ?" (Only (ticker_moeda ordem)) :: Handler [Only Int]

    -- Passo C: Salva o recibo no Histórico de Transações
    _ <- liftIO $ execute conn 
           "INSERT INTO transacoes (usuario_id, criptomoeda_id, tipo, quantidade, preco_unitario, taxa_aplicada) \
           \VALUES (1, ?, 'COMPRA', ?, ?, 0.0)"
           (idMoeda, quantidade ordem, preco_compra ordem)

    -- Passo D: Atualiza a Carteira (Calculando o preço médio matematicamente no SQL)
    _ <- liftIO $ execute conn
           "INSERT INTO carteiras (usuario_id, criptomoeda_id, quantidade, preco_medio_compra) \
           \VALUES (1, ?, ?, ?) \
           \ON CONFLICT (usuario_id, criptomoeda_id) DO UPDATE \
           \SET preco_medio_compra = ((carteiras.quantidade * carteiras.preco_medio_compra) + (EXCLUDED.quantidade * EXCLUDED.preco_medio_compra)) / (carteiras.quantidade + EXCLUDED.quantidade), \
           \    quantidade = carteiras.quantidade + EXCLUDED.quantidade"
           (idMoeda, quantidade ordem, preco_compra ordem)

    pure (ResultadoResponse "Ordem executada com sucesso!")

-- 2. EXIBIR CARTEIRA (GET)
handlerGetCriptos :: Connection -> Handler [AtivoCarteira]
handlerGetCriptos conn = do
    -- Junta a tabela de carteiras com a de criptomoedas para pegar os nomes
    lista <- liftIO $ query_ conn 
           "SELECT c.id, m.nome, m.ticker, c.quantidade, c.preco_medio_compra \
           \FROM carteiras c \
           \JOIN criptomoedas m ON c.criptomoeda_id = m.id \
           \WHERE c.usuario_id = 1 ORDER BY c.id ASC"
    pure lista

-- 3. VENDER/LIQUIDAR POSIÇÃO (DELETE)
handlerDeleteCripto :: Connection -> Int -> Handler ResultadoResponse
handlerDeleteCripto conn idCarteira = do
    _ <- liftIO $ execute conn "DELETE FROM carteiras WHERE id = ?" (Only idCarteira)
    pure (ResultadoResponse "Posição liquidada com sucesso!")

app :: Connection -> Application
app conn = serve (Proxy @API) (server conn)