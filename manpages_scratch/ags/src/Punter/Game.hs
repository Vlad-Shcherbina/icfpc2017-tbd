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

data Handshake = Handshake { me :: Text }
    deriving (Generic, FromJSON, ToJSON, Show)
data HandshakeAck = HandshakeAck { you :: Text }
    deriving (Generic, FromJSON, ToJSON, Show)

data SetupReq = SetupReq { u_punter   :: Integer
                         , u_punters  :: Integer
                         , u_map      :: Map
                         , u_settings :: Settings }
    deriving (Generic, Show)

data SetupRes = SetupRes { a_ready   :: Integer
                         , a_futures :: Maybe [Future] }
    deriving (Generic, Show)

data MoveReq = MoveInfo { move :: Moves }
             | ScoreInfo
    deriving (Generic, FromJSON, ToJSON, Show)
data Moves = Moves { moves :: [Move] }
    deriving (Generic, FromJSON, ToJSON, Show)
data Move = Pass { m_pass :: PassingPunter }
          | Claim { m_claim :: ClaimingPunter }
    deriving (Generic, FromJSON, ToJSON, Show)

newtype MoveRes = MoveRes Move
    deriving (Generic, FromJSON, ToJSON, Show)

-- Boring boilerplate

data PassingPunter = PassingPunter { p_punter :: Integer }
    deriving (Generic, Show)
data ClaimingPunter = ClaimingPunter { c_punter :: Integer
                                     , c_source :: Integer
                                     , c_target :: Integer }
    deriving (Generic, Show)

instance FromJSON ClaimingPunter where
    parseJSON = genericParseJSON opts
instance FromJSON PassingPunter where
    parseJSON = genericParseJSON opts
instance ToJSON ClaimingPunter where
    toJSON = genericToJSON opts
instance ToJSON PassingPunter where
    toJSON = genericToJSON opts
instance FromJSON SetupRes where
    parseJSON = genericParseJSON opts
instance ToJSON SetupRes where
    toJSON = genericToJSON opts
instance FromJSON Settings where
    parseJSON = genericParseJSON opts
instance ToJSON Settings where
    toJSON = genericToJSON opts
instance FromJSON SetupReq where
    parseJSON = genericParseJSON opts
instance ToJSON SetupReq where
    toJSON = genericToJSON opts
