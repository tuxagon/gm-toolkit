{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Api (runServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int64)
import Data.Text
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


import Database (fetchPostgresConnection, fetchProduct, createProduct)
import Schema

-- data Product = Product
--   { productName     :: Text
--   , productPrice    :: Int
--   , productQuantity :: Int
--   , productSource   :: Text
--   } deriving (Eq, Generic, Show)

-- instance ToJSON Product

type API =
       "api" :> "products" :> Get '[JSON] [Product]
  :<|> "api" :> "products" :> Capture "productid" Int64 :> Get '[JSON] Product
  :<|> "api" :> "products" :> ReqBody '[JSON] Product :> Post '[JSON] Product

fetchProductsHandler :: ConnectionString -> Handler [Product]
fetchProductsHandler connString =
  undefined

fetchProductHandler :: ConnectionString -> Int64 -> Handler Product
fetchProductHandler connString productId = do
  maybeProduct <- liftIO $ fetchProduct connString productId
  case maybeProduct of
    Just product -> return product
    Nothing -> Handler (throwE $ err401 { errBody = "Could not find product with that ID" })

createProductHandler :: ConnectionString -> Product -> Handler Product
createProductHandler connString product = do
  productId <- liftIO $ createProduct connString product
  maybeProduct <- liftIO $ fetchProduct connString productId
  case maybeProduct of
    Just product -> return product
    Nothing -> Handler (throwE $ err401 { errBody = "Could not find product with that ID" })

runServer :: IO ()
runServer = do
  connString <- fetchPostgresConnection
  run 8080 (serve api (server connString))

api :: Proxy API
api = Proxy :: Proxy API

server :: ConnectionString -> Server API
server connString =
  fetchProductsHandler connString :<|>
  fetchProductHandler connString :<|>
  createProductHandler connString
