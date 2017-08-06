{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Punter.Game where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Map      as M
import           Data.Text     hiding (concat)
import           GHC.Generics
import           Punter.Aeson
import           Punter.Map

data Punter = Punter { id       :: Integer
                     , name     :: Text
                     , claimed  :: [River]
                     , futures  :: [Future]
                     , splurges :: Integer }
    deriving Show

data Game = Game { players :: M.Map Integer Punter
                 , turns   :: Integer
                 , turn    :: Integer
                 , mines   :: [Mine]
                 , sites   :: [Site]
                 , free    :: [River]
                 , moves   :: [Move] }
    deriving Show

data Future = Future { f_source :: Integer
                     , f_target :: Integer }
    deriving (Generic, Show)

data Settings = Settings { s_futures  :: Bool
                         , s_splurges :: Bool }
    deriving (Generic, Show)

data Handshake = Handshake { h_me :: Text }
    deriving (Generic, Show)
data HandshakeAck = HandshakeAck { h_you :: Text }
    deriving (Generic, Show)

data SetupReq = SetupReq { u_punter   :: Integer
                         , u_punters  :: Integer
                         , u_map      :: Map
                         , u_settings :: Settings }
    deriving (Generic, Show)

data SetupRes = SetupRes { a_ready   :: Integer
                         , a_futures :: Maybe [Future] }
    deriving (Generic, Show)

data MoveReq = MoveInfo { mRmove :: Moves }
             | ScoreInfo { mRstop :: Stop }
    deriving (Generic, Show)
data Moves = Moves { v_moves :: [Move] }
    deriving (Generic, Show)
data Move = Pass { m_pass :: PassingPunter }
          | Claim { m_claim :: ClaimingPunter }
          | Splurge { m_splurge :: SplurgingPunter }
    deriving (Generic, Show)
data Stop = Stop { p_moves  :: [Move]
                 , p_scores :: Scores }
    deriving (Generic, Show)
data Scores = Scores { o_scores :: [Score] }
    deriving (Generic, Show)
data Score = Score { r_punter :: Integer
                   , r_score  :: Integer }
    deriving (Generic, Show)
data PassingPunter = PassingPunter { p_punter :: Integer }
    deriving (Generic, Show)
data ClaimingPunter = ClaimingPunter { c_punter :: Integer
                                     , c_source :: Integer
                                     , c_target :: Integer }
    deriving (Generic, Show)
data SplurgingPunter = SplurgingPunter { s_punter :: Integer
                                       , s_route  :: [Integer] }
    deriving (Generic, Show)

newtype MoveRes = MoveRes Move
    deriving (Generic, Show)

--

concat <$> mapM (deriveJSON opts) [ ''Handshake, ''HandshakeAck
                                  , ''Settings
                                  , ''Future
                                  , ''SetupReq, ''SetupRes
                                  , ''PassingPunter, ''ClaimingPunter
                                  , ''SplurgingPunter
                                  , ''Move
                                  , ''Score
                                  , ''Scores
                                  , ''Moves, ''Stop
                                  , ''MoveReq, ''MoveRes
                                  ]
