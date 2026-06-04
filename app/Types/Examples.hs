{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Examples where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

data OrdemCompra = OrdemCompra { nome_moeda :: String, ticker_moeda :: String, quantidade :: Double, preco_compra :: Double } deriving (Show, Generic)
instance FromJSON OrdemCompra
instance ToJSON OrdemCompra

data AtivoCarteira = AtivoCarteira { id_carteira :: Int, nome :: String, ticker :: String, qtd_total :: Double, preco_medio :: Double } deriving (Show, Generic)
instance FromJSON AtivoCarteira
instance ToJSON AtivoCarteira
instance FromRow AtivoCarteira where fromRow = AtivoCarteira <$> field <*> field <*> field <*> field <*> field

data ResultadoResponse = ResultadoResponse { resultado :: String } deriving (Show, Generic)
instance FromJSON ResultadoResponse
instance ToJSON ResultadoResponse

-- Novos Tipos para Autenticação e Saldo
data Usuario = Usuario { saldo_brl :: Double } deriving (Show, Generic)
instance FromJSON Usuario
instance ToJSON Usuario
instance FromRow Usuario where fromRow = Usuario <$> field

data Deposito = Deposito { valor :: Double } deriving (Show, Generic)
instance FromJSON Deposito
instance ToJSON Deposito