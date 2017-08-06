{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
where

import Data.Aeson

import GHC.Generics
import System.Directory
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Control.Monad.Fail as Fail
import Control.Monad as CM

import System.Random

type GameType = T.Text
type Key = T.Text

path = "./data/"
keys = path ++ "keys/"
maps = path ++ "maps/"

data RunningGame = RunningGame {
    creator :: Key
  , port :: Int
} deriving (Show, Generic)

instance FromJSON RunningGame
instance ToJSON RunningGame

createGame :: GameType -> Key -> IO (Maybe RunningGame)
createGame _ key = do
  port <- randomRIO (20000, 42000)
  let game = RunningGame key port
  let dataPath = maps ++ (show port)
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
    Nothing -> Fail.fail $ "Could not decode game data"

listValidGames :: IO [RunningGame]
listValidGames = do
  list <- listDirectory maps
  allGames <- mapM loadGame list
  CM.filterM (\g -> keyIsValid (creator g)) allGames

keyIsValid :: Key -> IO Bool
keyIsValid key = do
  doesFileExist (keys ++ (T.unpack key))
