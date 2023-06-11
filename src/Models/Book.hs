{-# LANGUAGE TemplateHaskell #-}

module Models.Book
  ( Book(..)
  ) where

import Data.Aeson
import Data.Aeson.TH

data Book =
  Book
    { id :: Maybe String
    , name :: String
    , author :: String
    }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Book)
