{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Punter.Map where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           GHC.Generics

data Map = Map { sites  :: [Site]
               , rivers :: [River]
               , mines  :: [Mine] }
    deriving (Generic, ToJSON, FromJSON, Show, Eq)

data Site = Site { id :: Integer }
    deriving (Generic, ToJSON, FromJSON, Show, Eq)

data River = River { source :: Integer
                   , target :: Integer }
    deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype Mine = Mine Integer
    deriving (Generic, ToJSON, FromJSON, Show, Eq)

exampleMap :: BS.ByteString
exampleMap = BS.concat
                    [ "{\"sites\":[{\"id\":4},{\"id\":1},{\"id\":3},{\"id\":6},{\"id\":5},{\"id\":0},{\"id\":7},{\"id\":2}],"
                    , "\"rivers\":[{\"source\":3,\"target\":4},{\"source\":0,\"target\":1},{\"source\":2,\"target\":3},"
                    , "          {\"source\":1,\"target\":3},{\"source\":5,\"target\":6},{\"source\":4,\"target\":5},"
                    , "          {\"source\":3,\"target\":5},{\"source\":6,\"target\":7},{\"source\":5,\"target\":7},"
                    , "          {\"source\":1,\"target\":7},{\"source\":0,\"target\":7},{\"source\":1,\"target\":2}],"
                    , "\"mines\":[1,5]}" ]

