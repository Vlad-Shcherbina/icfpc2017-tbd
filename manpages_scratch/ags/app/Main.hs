{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.HTTP.Types.Status

import Control.Monad
import Control.Monad.IO.Class
import qualified Network.Wai.Handler.Warp as W

import qualified Data.Text.Lazy as T

import Lib
import Database

opts :: Options
opts = Options 0 (W.setHost "0.0.0.0" W.defaultSettings)

launchGame :: RunningGame -> IO ()
launchGame g = do
  putStrLn $ "Starting game: " ++ show g

recreateGames :: IO ()
recreateGames = do
  games <- listValidGames
  mapM_ launchGame games

ifAuthorized :: (Key -> ActionM ()) -> ActionM ()
ifAuthorized action = do
  k <- param "key"
  valid <- liftIO $ keyIsValid k
  case valid of
    False -> status unauthorized401
    True -> action k

main :: IO ()
main = do
  recreateGames
  scottyOpts opts $ do
    get "/key/:key/scoreboard" $ do
      ifAuthorized renderScoreBoard
    get "/key/:key/map/:type/create" $
      ifAuthorized createAction

renderScoreBoard :: Key -> ActionM ()
renderScoreBoard key = do
  text "Alelua"

createAction :: Key -> ActionM ()
createAction key = do
  t <- param "type"
  valid <- liftIO $ keyIsValid key
  case valid of
    False -> status unauthorized401
    True -> do
      game <- liftIO $ createGame t key
      case game of
        Just g -> do
          liftIO $ launchGame g
          text $ T.pack $ show $ port g
        Nothing -> status serviceUnavailable503
