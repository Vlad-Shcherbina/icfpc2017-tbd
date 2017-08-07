{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.HTTP.Types.Status

import Control.Monad
import Control.Monad.IO.Class
import qualified Network.Wai.Handler.Warp as W

import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as Map
import Punter.TCPTransport
import Punter.Database

import qualified Pages as Pages

opts :: Options
opts = Options 0 (W.setHost "0.0.0.0" W.defaultSettings)

launchGame :: RunningGame -> IO ()
launchGame g = do
  putStrLn $ "Starting game: " ++ show g
  spawn (puntersQty g) (port g) (mapName g)

recreateGames :: IO ()
recreateGames = do
  games <- listValidGames
  mapM_ launchGame games

ifAuthorized :: (Key -> ActionM ()) -> ActionM ()
ifAuthorized action = do
  k <- param "key"
  valid <- liftIO $ keyIsValid k
  if valid then action k else status unauthorized401

main :: IO ()
main = do
  recreateGames
  scottyOpts opts $ do
    get "/key/:key/scoreboard/port/:port" $
      ifAuthorized renderScoreBoardPerPort
    get "/key/:key/scoreboard" $
      ifAuthorized renderScoreBoard
    get "/key/:key/map/:name/punters/:punters/create" $
      ifAuthorized createAction

renderScoreBoardPerPort :: Key -> ActionM ()
renderScoreBoardPerPort key = do
  port <- param "port"
  sc <- liftIO $ loadScores port
  Pages.scoreBoardPerPort port $ map (\(e, m) -> (e, scores m)) sc

renderScoreBoard :: Key -> ActionM ()
renderScoreBoard key = do
  gms <- liftIO $ gamesPerKey key
  Pages.scoreBoard $ map (\g -> (port g, mapName g)) gms

createAction :: Key -> ActionM ()
createAction key = do
  n <- param "name"
  p <- param "punters"
  valid <- liftIO $ keyIsValid key
  case valid of
    False -> status unauthorized401
    True -> do
      game <- liftIO $ createGame n p key
      case game of
        Just g -> do
          liftIO $ launchGame g
          text $ T.pack $ show $ port g
        Nothing -> status serviceUnavailable503
