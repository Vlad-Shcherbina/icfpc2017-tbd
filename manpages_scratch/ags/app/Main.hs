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

main :: IO ()
main = do
  scottyOpts opts $ do
    get "/map/:type/key/:key" $ do
      t <- param "type"
      k <- param "key"
      port <- liftIO $ portForGame t k
      text $ T.pack $ show port
