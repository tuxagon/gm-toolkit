{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where


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
