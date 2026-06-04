{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Examples where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

-- O que o Frontend envia (A Ordem de Compra)
data OrdemCompra = OrdemCompra
    { nome_moeda   :: String
    , ticker_moeda :: String
    , quantidade   :: Double
    , preco_compra :: Double
    } deriving (Show, Generic)

instance FromJSON OrdemCompra
instance ToJSON OrdemCompra

-- O que o Backend devolve para montar a tabela (Ativo da Carteira)
data AtivoCarteira = AtivoCarteira
    { id_carteira :: Int
    , nome        :: String
    , ticker      :: String
    , qtd_total   :: Double
    , preco_medio :: Double
    } deriving (Show, Generic)

instance FromJSON AtivoCarteira
instance ToJSON AtivoCarteira

instance FromRow AtivoCarteira where
    fromRow = AtivoCarteira <$> field <*> field <*> field <*> field <*> field

-- Resposta simples de sucesso
data ResultadoResponse = ResultadoResponse
    { resultado :: String 
    } deriving (Show, Generic)

instance FromJSON ResultadoResponse
instance ToJSON ResultadoResponse