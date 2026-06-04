{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Examples where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

-- Adicionamos quantidade e preco_medio
data Cripto = Cripto
    { id_cripto   :: Int
    , nome        :: String
    , ticker      :: String
    , quantidade  :: Double
    , preco_medio :: Double
    } deriving (Show, Generic)

instance FromJSON Cripto
instance ToJSON Cripto

-- Agora o FromRow precisa mapear 5 campos em vez de 3
instance FromRow Cripto where
    fromRow = Cripto <$> field <*> field <*> field <*> field <*> field

data ResultadoResponse = ResultadoResponse
    { resultado :: Int 
    } deriving (Show, Generic)

instance FromJSON ResultadoResponse
instance ToJSON ResultadoResponse