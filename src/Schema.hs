{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Schema where

import           Data.Aeson (ToJSON, FromJSON, Object, (.=), (.:), toJSON, fromJSON, parseJSON, object, withObject)
import           Data.Aeson.Types (Parser, Pair)
import qualified Database.Persist.TH as PTH

import Data.Text

PTH.share
  [PTH.mkPersist PTH.sqlSettings
  , PTH.mkMigrate "migrateAll"
  ]
  [PTH.persistLowerCase|
    Product sql=products
      name Text
      price Int
      quantity Int
      source Text
      deriving Show Read
  |]

instance ToJSON Product where
  toJSON product = object
    [ "name" .= productName product
    , "price" .= productPrice product
    , "quantity" .= productQuantity product
    , "source" .= productSource product
    ]

instance FromJSON Product where
  parseJSON = withObject "Product" parseProduct

parseProduct :: Object -> Parser Product
parseProduct p = do
  pName <- p .: "name"
  pPrice <- p .: "price"
  pQuantity <- p .: "quantity"
  pSource <- p .: "source"
  return Product
    { productName = pName
    , productPrice = pPrice
    , productQuantity = pQuantity
    , productSource = pSource
    }

