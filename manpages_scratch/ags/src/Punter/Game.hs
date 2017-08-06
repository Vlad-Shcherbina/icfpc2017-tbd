{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Punter.Game where

import           Data.Aeson
import qualified Data.Map     as M
import           Data.Text
import           GHC.Generics
import           Punter.Aeson
import           Punter.Map

data Punter = Punter { name    :: Text
                     , futures :: [Future] }
    deriving Show

data Game = Game { players :: M.Map Integer Punter
                 , turns   :: Integer
                 , turn    :: Integer
                 , free    :: [River]
                 , claimed :: [(River, Integer)] }
    deriving Show

data Future = Future { source :: Integer
                     , target :: Integer }
    deriving (Generic, FromJSON, ToJSON, Show)

data Settings = Settings { s_futures :: Bool }
    deriving (Generic, Show)
instance FromJSON Settings where
    parseJSON = genericParseJSON opts
instance ToJSON Settings where
    toJSON = genericToJSON opts

data HandshakeReq = HandshakeReq { me :: Text }
    deriving (Generic, FromJSON, ToJSON, Show)
data HandshakeRes = HandshakeRes { you :: Text }
    deriving (Generic, FromJSON, ToJSON, Show)

data Setup = Setup { u_punter   :: Integer
                   , u_punters  :: Integer
                   , u_map      :: Map
                   , u_settings :: Settings }
    deriving (Generic, Show)
instance FromJSON Setup where
    parseJSON = genericParseJSON opts
instance ToJSON Setup where
    toJSON = genericToJSON opts

data SetupAck = SetupAck { a_ready   :: Integer
                         , a_futures :: Maybe [Future] }
    deriving (Generic, Show)
instance FromJSON SetupAck where
    parseJSON = genericParseJSON opts
instance ToJSON SetupAck where
    toJSON = genericToJSON opts
