{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Punter.Database
where

import Data.Aeson

import GHC.Generics
import System.Directory
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Control.Monad as CM

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Time
import System.Random

type MapName = T.Text
type Key = T.Text
type PuntersQty = Int

path = "./data/"
keys = path ++ "keys/"
maps = path ++ "maps/"
score = path ++ "scores/"

data RunningGame = RunningGame {
    creator :: Key
  , port :: Int
  , mapName :: String
  , puntersQty :: Int
} deriving (Show, Generic)

instance FromJSON RunningGame
instance ToJSON RunningGame

data GameScore = GameScore {
    scores :: Map.Map String Integer
} deriving (Show, Generic)
instance FromJSON GameScore
instance ToJSON GameScore

saveScore :: Int -> GameScore -> IO ()
saveScore port sc = do
  t <- getCurrentTime
  let enc = encode sc
  let d = score ++ show port ++ "/"
  createDirectoryIfMissing True d
  BSL.writeFile (d ++ show t) enc

loadScores :: Int -> IO [(String, GameScore)]
loadScores port = do
  let d = score ++ show port ++ "/"
  list <- fmap List.sort $ listDirectory d
  mapM (loadScore port) (reverse list)

loadScore :: Int -> FilePath -> IO (String, GameScore)
loadScore po p = do
  s <- BSL.readFile $ score ++ show po ++ "/" ++ p
  case decode s of
    Just o -> return (p, o)
    Nothing -> error "Could not decode scores"

createGame :: MapName -> PuntersQty -> Key -> IO (Maybe RunningGame)
createGame mapName puntersQty key = do
  port <- randomRIO (20000, 42000)
  let game = RunningGame key port (T.unpack mapName) puntersQty
  let dataPath = maps ++ (show port)
  createDirectoryIfMissing True maps
  exists <- doesFileExist dataPath
  case exists of
    True -> return Nothing
    False -> do
      BSL.writeFile dataPath $ encode game
      return $ Just game

loadGame :: FilePath -> IO RunningGame
loadGame f = do
  c <- BSL.readFile $ maps ++ f
  case decode c of
    Just o -> return o
    Nothing -> error "Could not decode game data"

listValidGames :: IO [RunningGame]
listValidGames = do
  list <- listDirectory maps
  allGames <- mapM loadGame list
  CM.filterM (\g -> keyIsValid (creator g)) allGames

keyIsValid :: Key -> IO Bool
keyIsValid key = do
  doesFileExist (keys ++ (T.unpack key))

gamesPerKey :: Key -> IO [RunningGame]
gamesPerKey key = do
  games <- listValidGames
  return $ filter (\g -> creator g == key) games
