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

main :: IO ()
main = do
  recreateGames
  scottyOpts opts $ do
    get "/key/:key/map/:type" $ do
      t <- param "type"
      k <- param "key"
      valid <- liftIO $ keyIsValid k
      case valid of
        False -> status unauthorized401
        True -> do
          game <- liftIO $ createGame t k
          case game of
            Just g -> do
              liftIO $ launchGame g
              text $ T.pack $ show $ port g
            Nothing -> status serviceUnavailable503
