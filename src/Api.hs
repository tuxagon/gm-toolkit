{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Api
    ( startApp
    , app
    ) where

import qualified Model as M

import Data.Aeson
import Data.Aeson.TH
import Data.Text
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Product = Product
  { productName     :: Text
  , productPrice    :: Int
  , productQuantity :: Int
  , productSource   :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON Product

type API = "products" :> Get '[JSON] [Product]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return products

products :: [Product]
products =
  [ Product "Sword" 50 2 "You hit things with it"
  ]
