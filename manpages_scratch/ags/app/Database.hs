{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
where

import Data.Aeson

import GHC.Generics
import System.Directory
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Monad.Fail as Fail
import Control.Monad as CM

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

portForGame :: GameType -> Key -> IO Int
portForGame _ _ = return 4444

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
  list <- listDirectory keys
  return $ key `elem` (fmap T.pack list)
