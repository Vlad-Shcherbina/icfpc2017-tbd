{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Punter.Game where

import           Data.Aeson.TH
import qualified Data.Map      as M
import           Data.Text     hiding (concat)
import           GHC.Generics
import           Punter.Aeson
import           Punter.Map

data Punter = Punter { pId      :: Integer
                     , name     :: Text
                     , claimed  :: [River]
                     , futures  :: [Future]
                     , splurges :: Integer }
    deriving (Show, Eq)

data Game = Game { g_players :: M.Map Integer Punter
                 , g_slots   :: Integer
                 , g_turns   :: Integer
                 , g_turn    :: Integer
                 , g_mines   :: [Mine]
                 , g_sites   :: [Site]
                 , g_free    :: [River]
                 , g_moves   :: [Move] }
    deriving (Show, Eq)

mkGame :: Integer -> Map -> Game
mkGame playerCount (Map{sites, rivers, mines}) =
    Game { g_players = M.empty
         , g_slots   = playerCount
         , g_turns   = 0
         , g_turn    = 0
         , g_mines   = mines
         , g_sites   = sites
         , g_free    = rivers
         , g_moves   = [] }

data Future = Future { f_source :: Integer
                     , f_target :: Integer }
    deriving (Generic, Show, Eq)

data Settings = Settings { s_futures  :: Bool
                         , s_splurges :: Bool }
    deriving (Generic, Show, Eq)

data Handshake = Handshake { h_me :: Text }
    deriving (Generic, Show, Eq)
data HandshakeAck = HandshakeAck { h_you :: Text }
    deriving (Generic, Show, Eq)

data SetupReq = SetupReq { u_punter   :: Integer
                         , u_punters  :: Integer
                         , u_map      :: Map
                         , u_settings :: Settings }
    deriving (Generic, Show, Eq)

data SetupRes = SetupRes { a_ready   :: Integer
                         , a_futures :: Maybe [Future] }
    deriving (Generic, Show, Eq)

passingMove :: Integer -> Move
passingMove p = Pass (PassingPunter p)

claimingMove :: Integer -> River -> Move
claimingMove p (River {source, target}) =
    Claim (ClaimingPunter p source target)

splurgingMove :: Integer -> [Integer] -> Move
splurgingMove p route =
    Splurge (SplurgingPunter p route)

data MoveReq = MoveInfo { mRmove :: Moves }
             | ScoreInfo { mRstop :: Stop }
    deriving (Generic, Show, Eq)
data Moves = Moves { v_moves :: [Move] }
    deriving (Generic, Show, Eq)
data Move = Pass { m_pass :: PassingPunter }
          | Claim { m_claim :: ClaimingPunter }
          | Splurge { m_splurge :: SplurgingPunter }
    deriving (Generic, Show, Eq)
data Stop = Stop { p_moves  :: [Move]
                 , p_scores :: Scores }
    deriving (Generic, Show, Eq)
data Scores = Scores { o_scores :: [Score] }
    deriving (Generic, Show, Eq)
data Score = Score { r_punter :: Integer
                   , r_score  :: Integer }
    deriving (Generic, Show, Eq)
data PassingPunter = PassingPunter { p_punter :: Integer }
    deriving (Generic, Show, Eq)
data ClaimingPunter = ClaimingPunter { c_punter :: Integer
                                     , c_source :: Integer
                                     , c_target :: Integer }
    deriving (Generic, Show, Eq)
data SplurgingPunter = SplurgingPunter { s_punter :: Integer
                                       , s_route  :: [Integer] }
    deriving (Generic, Show, Eq)

newtype MoveRes = MoveRes Move
    deriving (Generic, Show, Eq)

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
